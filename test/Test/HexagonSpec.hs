module Test.HexagonSpec
  ( spec
  )
where

import           Data.Foldable
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
import State
import Combat

spec :: Spec
spec = do
  describe "Axial to pixel and vice versa" $ do
    it "Axial should be isomorphic to Point V2 Cint" $ property isoAxial
    it "Point V2 Cint should be isomorphic to Axial" $ property isoPoint

  describe "neighbor function" $ do
    it "should never generate more then 6" $ property $ \x -> length (neigbours x) <= 6
    it "should be bigger or equal to zero" $ property $ \x -> length (neigbours x) >= 0
    it "diff should be no bigger then one" $ property tileDiffNoBiggerThenOne
    it "neighbors should be in grid " $ property niegBourInGrid
    it "1,1 always has 6 neighbors" $ length (neigbours (MkAxial 1 1)) `shouldBe` 6
  describe "move" $ do
    it "character should move" $
      shouldCharacterMove testState (MkAxial 3 3) `shouldBe`
        (Just $ MkWalk $ MkMove (MkAxial 2 3) (MkAxial 3 3) (playerUnit))
    it "should clear where we're moving from" $
      forAll (suchThat arbitrary (uncurry moveInGrid)) moveBecomesEmpty
    -- TODO too slow
    -- it "should set destination to origin val" $
    --   forAll (suchThat arbitrary (uncurry moveInGrid)) moveFromMoveIsToContent
-- moveFromMoveIsToContent :: (Grid, Move) -> Bool
-- moveFromMoveIsToContent (grid, mv) =
--    (res ^? at (mv ^. move_to) . _Just . tile_content )
--    == (grid ^? at (mv ^. move_from) . _Just . tile_content)
--    where
--      res = move mv grid


moveInGrid :: Grid -> Move -> Bool
moveInGrid grid mv =
  has (at (mv ^. move_from) . _Just . tile_content . _Just) grid
  &&
  has (at (mv ^. move_to) . _Just) grid

moveBecomesEmpty :: (Grid, Move) -> Property
moveBecomesEmpty (grid, mv) =
  counterexample (printf "input: %s, output %s:" gridInput gridOutput) $
    has (at (mv ^. move_from) . _Just . tile_content . _Nothing) result

  where
    result = move (MkWalk mv) grid mv
    gridInput = show (grid ^.. contentFold)
    gridOutput= show (result ^.. contentFold)


testState :: GameState
testState = set game_selected (Just playerAxial) $ MkGameState Nothing $ level initialGrid

playerAxial :: Axial
playerAxial = MkAxial 2 3

playerUnit :: Unit
playerUnit = defUnit

level :: Grid -> Grid
level = fold
  [ at playerAxial . _Just . tile_content ?~ Player playerUnit
  ]

niegBourInGrid :: Axial -> Property
niegBourInGrid x =
  counterexample (printf "output neigbours: %s" $ show neighs ) $
    all (`elem` (fmap fst $ Map.toList $ initialGrid)) neighs
  where
      neighs = neigbours x

tileDiffNoBiggerThenOne :: Axial -> Bool
tileDiffNoBiggerThenOne x =
  flip all (neigbours x) $ \y -> let
      qdiff = abs ((x ^. _q) - (y ^. _q))
      rdiff = abs ((x ^. _r) - (y ^. _r))
    in
    (qdiff == 1 || qdiff == 0) && (rdiff == 1 || rdiff == 0)


isoAxial :: Axial -> Property
isoAxial tile = counterexample (printf "actual result: %s" $ show other) $
  other == tile
  where
    other = pixelToAxial $ axialToPixel tile

isoPoint :: Point V2 CInt -> Property
isoPoint point' = counterexample (printf "actual result: %s" $ show other) $
  other + (P $ V2 (fromIntegral hexSize) $ fromIntegral hexSize) > point' &&
    other - (P $ V2 (fromIntegral hexSize) $ fromIntegral hexSize) < point'
  where
    other = axialToPixel $ pixelToAxial point'
