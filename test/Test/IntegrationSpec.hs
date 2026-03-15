{-# LANGUAGE ScopedTypeVariables #-}

module Test.IntegrationSpec (spec) where

import           Data.IORef
import           Data.Word              (Word8)
import           Reflex.SDL2            (V4 (..), WindowExposedEventData (..))
import           Test.Hspec

import           Plunder.State          (initialState)
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

spec :: Spec
spec = beforeAll (withTestEnv $ \env -> do
    (handle, ref) <- bootApp env initialState
    fireWindowExposed handle (WindowExposedEventData (teWindow env))
    pure (handle, ref, env)
  ) $ do

  describe "initial render after WindowExposed" $ do

    -- 49 land tiles per render cycle × 3 cycles = 147
    it "renders 147 terrain polygons with land colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let landPolys = filter (isFillPolygonWithColor landColor) calls
      length landPolys `shouldBe` 147

    -- 32 water tiles (out-of-grid + (0,0)) per cycle × 3 = 96
    it "renders 96 terrain polygons with water colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let waterPolys = filter (isFillPolygonWithColor waterColor) calls
      length waterPolys `shouldBe` 96

    -- 25 unexplored tiles per cycle × 3 = 75
    it "renders 75 fog polygons with unexplored colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let unexplored = filter (isFillPolygonWithColor unexploredColor) calls
      length unexplored `shouldBe` 75

    -- 37 fog tiles per cycle × 3 = 111
    it "renders 111 fog polygons with semi-transparent colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let foggy = filter (isFillPolygonWithColor fogColor) calls
      length foggy `shouldBe` 111

    -- 29 copies per cycle × 3 = 87 (sprites + weapons + text labels)
    it "renders 87 copy calls for sprites and text" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let copies = filter isCopy calls
      length copies `shouldBe` 87

    -- 14 fillRects per cycle × 3 = 42 (health bars bg+fill + money bg + help bg)
    it "renders 42 fillRect calls for health bars and UI" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let fills = filter isFillRect calls
      length fills `shouldBe` 42

    it "renders exactly 3 clear and 3 present calls" $ \(_, ref, _) -> do
      calls <- readIORef ref
      filter isClear calls `shouldBe` [RcClear, RcClear, RcClear]
      filter isPresent calls `shouldBe` [RcPresent, RcPresent, RcPresent]
