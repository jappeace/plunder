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
    -- Dismiss the help screen by firing Enter, then trigger a redraw
    fireWindowExposed handle (WindowExposedEventData (teWindow env))
    pure (handle, ref, env)
  ) $ do

  describe "initial render after WindowExposed" $ do

    it "renders terrain polygons with land colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let landPolys = filter (isFillPolygonWithColor landColor) calls
      length landPolys `shouldSatisfy` (> 0)

    it "renders terrain polygons with water colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let waterPolys = filter (isFillPolygonWithColor waterColor) calls
      length waterPolys `shouldSatisfy` (> 0)

    it "renders fog overlay with unexplored colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let unexplored = filter (isFillPolygonWithColor unexploredColor) calls
      length unexplored `shouldSatisfy` (> 0)

    it "renders fog overlay with semi-transparent colour" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let foggy = filter (isFillPolygonWithColor fogColor) calls
      length foggy `shouldSatisfy` (> 0)

    it "renders sprites via copy calls" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let copies = filter isCopy calls
      length copies `shouldSatisfy` (> 0)

    it "renders health bars via fillRect calls" $ \(_, ref, _) -> do
      calls <- readIORef ref
      let fills = filter isFillRect calls
      length fills `shouldSatisfy` (> 0)

    it "includes a clear and present in the render cycle" $ \(_, ref, _) -> do
      calls <- readIORef ref
      filter isClear calls `shouldSatisfy` (not . null)
      filter isPresent calls `shouldSatisfy` (not . null)
