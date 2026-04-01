{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IntegrationSpec (spec) where

import           Control.Lens           (view, (.~), (&))
import           Control.Monad          (void)
import           Data.IORef
import           Data.Word              (Word8)
import           Reflex                 (ffor, performEvent_, updated)
import           Reflex.SDL2            (V4 (..), WindowExposedEventData (..),
                                         liftIO)
import           SDL                    (InputMotion (..), KeyModifier (..),
                                         Keysym (..), KeyboardEventData (..))
import           SDL.Input.Keyboard.Codes (pattern KeycodeReturn,
                                         pattern ScancodeReturn)
import           Test.Hspec

import           Plunder                (app)
import           Plunder.State          (GamePhase (..), game_board,
                                         game_inventory_open,
                                         game_phase, game_selected, game_shop,
                                         initialState, levelToGameState,
                                         selectedTileInfo, ContextInfo(..))
import           Plunder.Grid           (Axial(..), flankingPreview)
import           Plunder.Level          (Level(..), TilePlacement(..),
                                         TileContentDef(..))
import           Test.IntegrationExpected
import           Test.TestHost

-- | Predicate helpers for matching render calls.
isFillPolygonWithColor :: V4 Word8 -> RenderCall -> Bool
isFillPolygonWithColor c (RcFillPolygon _ _ c') = c == c'
isFillPolygonWithColor _ _                      = False

isCopy :: RenderCall -> Bool
isCopy (RcCopy _ _) = True
isCopy _            = False

isFillRect :: RenderCall -> Bool
isFillRect (RcFillRect _) = True
isFillRect _              = False

isDrawRect :: RenderCall -> Bool
isDrawRect (RcDrawRect _) = True
isDrawRect _              = False

isClear :: RenderCall -> Bool
isClear RcClear = True
isClear _       = False

isPresent :: RenderCall -> Bool
isPresent RcPresent = True
isPresent _         = False

-- | Terrain colours from Plunder.Render.Terrain
landColor :: V4 Word8
landColor = V4 86 168 86 255

waterColor :: V4 Word8
waterColor = V4 65 105 225 255

-- | Fog colours
fogColor :: V4 Word8
fogColor = V4 0 0 0 128

unexploredColor :: V4 Word8
unexploredColor = V4 0 0 0 255

-- | A keyboard event for pressing Enter with no modifiers.
enterKeyPress :: KeyboardEventData
enterKeyPress = KeyboardEventData
  Nothing
  Pressed
  False
  (Keysym ScancodeReturn KeycodeReturn noModifier)

-- | 'KeyModifier' with all fields set to 'False'.
noModifier :: KeyModifier
noModifier = KeyModifier False False False False False False False False False False False

spec :: Spec
spec = do
  describe "initial render after WindowExposed" $
    beforeAll (withTestEnv $ \env -> do
      (handle, ref) <- bootApp env (void $ app initialState)
      fireWindowExposed handle (WindowExposedEventData (teWindow env))
      pure (handle, ref, env)
    ) $ do

      it "renders land terrain polygons" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let landPolys = filter (isFillPolygonWithColor landColor) calls
        landPolys `shouldBe` expectedLandPolys

      it "renders water terrain polygons" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let waterPolys = filter (isFillPolygonWithColor waterColor) calls
        waterPolys `shouldBe` expectedWaterPolys

      it "renders unexplored fog overlay" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let unexplored = filter (isFillPolygonWithColor unexploredColor) calls
        unexplored `shouldBe` expectedUnexploredPolys

      it "renders fog overlay" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let foggy = filter (isFillPolygonWithColor fogColor) calls
        foggy `shouldBe` expectedFogPolys

      it "renders sprite copies" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let copies = filter isCopy calls
        copies `shouldBe` expectedCopies

      it "renders fill rects (health bars, UI)" $ \(_, ref, _) -> do
        calls <- readIORef ref
        let fills = filter isFillRect calls
        fills `shouldBe` expectedFillRects

      it "performs clear and present each render cycle" $ \(_, ref, _) -> do
        calls <- readIORef ref
        filter isClear calls `shouldBe` [RcClear, RcClear, RcClear]
        filter isPresent calls `shouldBe` [RcPresent, RcPresent, RcPresent]

  describe "after pressing Enter to dismiss help" $
    beforeAll (withTestEnv $ \env -> do
      stateRef <- newIORef initialState
      (handle, ref) <- bootApp env $ do
        dynGS <- app initialState
        performEvent_ $ ffor (updated dynGS) $ liftIO . writeIORef stateRef
      fireWindowExposed handle (WindowExposedEventData (teWindow env))
      initialCalls <- readIORef ref
      -- Clear the log then press Enter to dismiss the help overlay
      writeIORef ref []
      fireKeyboard handle enterKeyPress
      afterCalls <- readIORef ref
      gs <- readIORef stateRef
      pure (initialCalls, afterCalls, gs)
    ) $ do

      it "triggers a render cycle" $ \(_, afterCalls, _) -> do
        filter isClear afterCalls `shouldSatisfy` (not . null)
        filter isPresent afterCalls `shouldSatisfy` (not . null)

      it "no longer renders the help overlay border" $ \(initialCalls, afterCalls, _) -> do
        let initialDrawRects = length $ filter isDrawRect initialCalls
            afterDrawRects   = length $ filter isDrawRect afterCalls
        afterDrawRects `shouldSatisfy` (< initialDrawRects)

      it "renders fewer copies without help text" $ \(initialCalls, afterCalls, _) -> do
        let initialCopies = length $ filter isCopy initialCalls
            afterCopies   = length $ filter isCopy afterCalls
        afterCopies `shouldSatisfy` (< initialCopies)

      it "game state is still Playing with no selection" $ \(_, _, gs) -> do
        view game_phase gs `shouldBe` Playing
        view game_selected gs `shouldBe` Nothing
        view game_shop gs `shouldBe` Nothing
        view game_inventory_open gs `shouldBe` False

  describe "flanking context panel" $ do
    -- Build a custom level with 3 players adjacent to an enemy at (3,3).
    let flankLevel = MkLevel
          { _level_grid_begin = 0
          , _level_grid_end   = 6
          , _level_money      = 0
          , _level_tiles =
              [ MkTilePlacement 3 3 (Just (EnemyDef 10 Nothing)) Nothing Nothing
              , MkTilePlacement 2 3 (Just (PlayerDef 10 Nothing)) Nothing Nothing
              , MkTilePlacement 4 3 (Just (PlayerDef 10 Nothing)) Nothing Nothing
              , MkTilePlacement 3 4 (Just (PlayerDef 10 Nothing)) Nothing Nothing
              ]
          }
        flankGS = levelToGameState flankLevel

    it "flanking preview is non-zero with multiple players adjacent" $ do
      let board = view game_board flankGS
      -- 3 players adjacent to enemy at (3,3); preview subtracts 1 → 2 allies → 10%
      flankingPreview board (MkAxial 3 3) `shouldBe` 10

    it "selecting the flanked enemy shows non-zero flanking in ContextInfo" $ do
      let gs = flankGS & game_selected .~ Just (MkAxial 3 3)
      case selectedTileInfo gs of
        ContextEnemy _ _ flanking -> flanking `shouldBe` 10
        other -> expectationFailure $ "Expected ContextEnemy, got: " <> show other

    it "full app boots and enters Playing phase with flanking level" $
      withTestEnv $ \env -> do
        stateRef <- newIORef flankGS
        (handle, _ref) <- bootApp env $ do
          dynGS <- app flankGS
          performEvent_ $ ffor (updated dynGS) $ liftIO . writeIORef stateRef
        fireWindowExposed handle (WindowExposedEventData (teWindow env))
        fireKeyboard handle enterKeyPress
        gs <- readIORef stateRef
        view game_phase gs `shouldBe` Playing
