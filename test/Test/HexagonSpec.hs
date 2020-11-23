module Test.HexagonSpec
  ( spec
  )
where

import           Foreign.C.Types (CInt)
import           Game13          ()
import           Grid
import           Reflex.SDL2
import           Test.Hspec
import           Test.Orphanage ()
import           Test.QuickCheck
import           Text.Printf
import           Control.Lens
import    qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Tile to pixel and vice versa" $ do
    it "Tile should be isomorphic to Point V2 Cint" $ property isoTile
    it "Point V2 Cint should be isomorphic to Tile" $ property isoPoint
  describe "neigbour function" $ do
    it "should never generate more then 6" $ property $ \x -> length (neigbours x) <= 6
    it "should be bigger or equal to zero" $ property $ \x -> length (neigbours x) >= 0
    it "diff should be no bigger then one" $ property tileDiffNoBiggerThenOne
    it "neigbours should be in grid " $ property niegBourInGrid

niegBourInGrid :: Tile -> Property
niegBourInGrid x =
  counterexample (printf "ouptut neigbours: %s" $ show neighs ) $
    all (`elem` (fmap snd $ Map.toList $ unGrid initialGrid)) neighs
  where
      neighs = neigbours x

tileDiffNoBiggerThenOne :: Tile -> Bool
tileDiffNoBiggerThenOne x =
  flip all (neigbours x) $ \y -> let
      qdiff = abs ((x ^. _q) - (y ^. _q))
      rdiff = abs ((x ^. _r) - (y ^. _r))
    in
    (qdiff == 1 || qdiff == 0) && (rdiff == 1 || rdiff == 0)


isoTile :: Tile -> Property
isoTile tile = counterexample (printf "actual result: %s" $ show other) $
  other == tile
  where
    other = pixelToTile $ tileToPixel tile

isoPoint :: Point V2 CInt -> Property
isoPoint point' = counterexample (printf "actual result: %s" $ show other) $
  other + (P $ V2 (fromIntegral hexSize) $ fromIntegral hexSize) > point' &&
    other - (P $ V2 (fromIntegral hexSize) $ fromIntegral hexSize) < point'
  where
    other = tileToPixel $ pixelToTile point'
