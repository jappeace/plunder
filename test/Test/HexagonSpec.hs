module Test.HexagonSpec
  ( spec
  )
where

import           Foreign.C.Types (CInt)
import           Game13          ()
import           Grid
import           Hexagon
import           Reflex.SDL2
import           Test.Hspec
import           Test.Orphanage ()
import           Test.QuickCheck
import           Text.Printf

spec :: Spec
spec =
  describe "Tile to pixel and vice versa" $ do
    it "Tile should be isomorphic to Point V2 Cint" $ property isoTile
    it "Point V2 Cint should be isomorphic to Tile" $ property isoPoint

isoTile :: Tile -> Property
isoTile tile = counterexample (printf "actual result: %s" $ show other) $
  other == tile
  where
    other = pixelToTile $ tileToPixel tile

isoPoint :: Point V2 CInt -> Property
isoPoint point' = counterexample (printf "actual result: %s" $ show other) $
  other == point'
  where
    other = tileToPixel $ pixelToTile point'
