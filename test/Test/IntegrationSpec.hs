{-# LANGUAGE ScopedTypeVariables #-}

module Test.IntegrationSpec (spec) where

import           Data.IORef
import           Data.Word              (Word8)
import           Reflex.SDL2            (V4 (..), WindowExposedEventData (..))
import           Test.Hspec

import           Plunder.State          (initialState)
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
