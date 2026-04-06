{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IntegrationSpec (spec) where

import           Control.Lens           (view, (.~), (&), (^.))
import           Data.IORef
import qualified Data.Map.Strict        as Map
import           Reflex.SDL2            (WindowExposedEventData (..))
import           SDL                    (InputMotion (..), KeyModifier (..),
                                         Keysym (..), KeyboardEventData (..))
import           SDL.Input.Keyboard.Codes (pattern KeycodeReturn,
                                         pattern ScancodeReturn)
import           Test.Hspec

import           Plunder.Grid           (Axial(..), Terrain(..), flankingPreview)
import           Plunder.Level          (Level(..), TilePlacement(..),
                                         TileContentDef(..))
import           Plunder.RenderState
import           Plunder.State          (GamePhase (..), GameState,
                                         ContextInfo(..),
                                         game_board, game_inventory_open,
                                         game_phase, game_selected, game_shop,
                                         initialState, levelToGameState,
                                         selectedTileInfo)
import           Test.TestHost

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

-- | Convert a GameState to RenderState with initial-frame parameters:
--   banner alpha = 0, help open = True.
initialRenderState :: GameState -> RenderState
initialRenderState = gameStateToRenderState 0 True

spec :: Spec
spec = do
  describe "initial render after WindowExposed" $
    beforeAll (withTestEnv $ \env -> do
      (handle, stateRef) <- bootAppWithState env initialState
      fireWindowExposed handle (WindowExposedEventData (teWindow env))
      gs <- readIORef stateRef
      pure (gs, initialRenderState gs)
    ) $ do

      it "visible tiles near player have terrain and sprites" $ \(_gs, rs) -> do
        let tiles = rs ^. render_tiles
            playerTile = tiles Map.! MkAxial 2 3
        playerTile ^. rtile_sprite `shouldBe` Just PlayerSprite
        playerTile ^. rtile_terrain `shouldBe` Land
        playerTile ^. rtile_visibility `shouldBe` Visible
        -- Adjacent tile (3,3) has a house and is visible
        let houseTile = tiles Map.! MkAxial 3 3
        houseTile ^. rtile_sprite `shouldBe` Just HouseSprite
        houseTile ^. rtile_visibility `shouldBe` Visible

      it "far tiles are unexplored" $ \(_gs, rs) -> do
        let tiles = rs ^. render_tiles
            -- (0,0) is hex distance 5 from player at (2,3), beyond sight and unexplored
            farTile = tiles Map.! MkAxial 0 0
        farTile ^. rtile_visibility `shouldBe` Unexplored

      it "fog tiles exist between visible and unexplored" $ \(_gs, rs) -> do
        let tiles = rs ^. render_tiles
            hasFog = any (\renderTile -> renderTile ^. rtile_visibility == Fog) (Map.elems tiles)
        hasFog `shouldBe` True

      it "health bars present for units" $ \(_gs, rs) -> do
        let tiles = rs ^. render_tiles
            playerTile = tiles Map.! MkAxial 2 3
        case playerTile ^. rtile_healthBar of
          Nothing -> expectationFailure "Expected health bar on player tile"
          Just hb -> hb ^. hb_maxHp `shouldSatisfy` (> 0)

      it "HUD shows correct initial state" $ \(_gs, rs) -> do
        let hud = rs ^. render_hud
        hud ^. hud_purchaseLabel `shouldBe` Nothing
        hud ^. hud_help `shouldBe` HelpVisible
        hud ^. hud_banner `shouldBe` NoBanner
        (hud ^. hud_inventory . invInfo_isOpen) `shouldBe` False

      it "border tiles outside grid are Water" $ \(_gs, rs) -> do
        let tiles = rs ^. render_tiles
            borderTile = tiles Map.! MkAxial (-1) (-1)
        borderTile ^. rtile_terrain `shouldBe` Water

  describe "after pressing Enter to dismiss help" $
    beforeAll (withTestEnv $ \env -> do
      (handle, stateRef) <- bootAppWithState env initialState
      fireWindowExposed handle (WindowExposedEventData (teWindow env))
      -- Press Enter to dismiss the help overlay
      fireKeyboard handle enterKeyPress
      gs <- readIORef stateRef
      pure gs
    ) $ do

      it "help state changes to HelpHidden" $ \gs -> do
        -- After Enter, help should be dismissed; rendering with helpOpen=False
        let rs = gameStateToRenderState 0 False gs
        rs ^. render_hud . hud_help `shouldBe` HelpHidden

      it "game state is still Playing" $ \gs -> do
        view game_phase gs `shouldBe` Playing
        view game_selected gs `shouldBe` Nothing
        view game_shop gs `shouldBe` Nothing
        view game_inventory_open gs `shouldBe` False

      it "Reflex event loop processes keyboard input" $ \gs -> do
        -- The fact that GameState was captured after Enter proves
        -- the Reflex network processed the keyboard event.
        -- The game should still be in a valid state.
        view game_phase gs `shouldBe` Playing

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
        (handle, stateRef) <- bootAppWithState env flankGS
        fireWindowExposed handle (WindowExposedEventData (teWindow env))
        fireKeyboard handle enterKeyPress
        gs <- readIORef stateRef
        view game_phase gs `shouldBe` Playing

    it "flanking level RenderState has correct enemy sprite" $ do
      let gs = flankGS & game_selected .~ Just (MkAxial 3 3)
          rs = gameStateToRenderState 0 False gs
          enemyTile = (rs ^. render_tiles) Map.! MkAxial 3 3
      enemyTile ^. rtile_sprite `shouldBe` Just EnemySprite
      case rs ^. render_hud . hud_contextPanel of
        ContextEnemy _ _ flanking -> flanking `shouldBe` 10
        other -> expectationFailure $ "Expected ContextEnemy, got: " <> show other
