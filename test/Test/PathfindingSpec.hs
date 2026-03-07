module Test.PathfindingSpec(spec) where

import           Control.Lens
import           Plunder.Grid
import           Plunder.Pathfinding
import           Test.Hspec

-- | A small 7x7 grid (0..6) of all-Land empty tiles, matching the default level layout.
emptyGrid :: Grid
emptyGrid = mkGrid 0 6

spec :: Spec
spec = do
  describe "findPath" $ do
    it "returns empty path for src == dst" $
      findPath emptyGrid (MkAxial 2 3) (MkAxial 2 3) `shouldBe` Just []

    it "finds a one-step path to an adjacent tile" $
      findPath emptyGrid (MkAxial 2 3) (MkAxial 2 4) `shouldBe` Just [MkAxial 2 4]

    it "finds a two-step path through an empty intermediate" $ do
      let result = findPath emptyGrid (MkAxial 2 3) (MkAxial 2 5)
      result `shouldBe` Just [MkAxial 2 4, MkAxial 2 5]

    it "routes around an occupied intermediate tile" $ do
      let blocked = emptyGrid
            & ix (MkAxial 2 4) . tile_content ?~ Enemy defUnit
          result = findPath blocked (MkAxial 2 3) (MkAxial 2 5)
      -- Path must avoid (2,4) since it has content
      result `shouldSatisfy` \case
        Just path -> MkAxial 2 4 `notElem` path && last path == MkAxial 2 5
        Nothing   -> False

    it "rejects a path through Water terrain" $ do
      let waterGrid = emptyGrid
            & ix (MkAxial 2 4) . tile_terrain .~ Water
      -- If (2,4) is the only route to (2,5) in a straight line, BFS may find an alternate
      -- but if we block ALL neighbours except through water, it should fail.
      -- Block the straight path: surround (2,5) with Water except through (2,4)
      let fullyBlocked = waterGrid
            & ix (MkAxial 1 5) . tile_terrain .~ Water
            & ix (MkAxial 3 4) . tile_terrain .~ Water
            & ix (MkAxial 1 6) . tile_terrain .~ Water
            & ix (MkAxial 3 5) . tile_terrain .~ Water
            & ix (MkAxial 2 6) . tile_terrain .~ Water
      findPath fullyBlocked (MkAxial 2 3) (MkAxial 2 5) `shouldBe` Nothing

    it "rejects a path through Mountains terrain" $ do
      let mtnGrid = emptyGrid
            & ix (MkAxial 2 4) . tile_terrain .~ Mountains
            & ix (MkAxial 1 5) . tile_terrain .~ Mountains
            & ix (MkAxial 3 4) . tile_terrain .~ Mountains
            & ix (MkAxial 1 6) . tile_terrain .~ Mountains
            & ix (MkAxial 3 5) . tile_terrain .~ Mountains
            & ix (MkAxial 2 6) . tile_terrain .~ Mountains
      findPath mtnGrid (MkAxial 2 3) (MkAxial 2 5) `shouldBe` Nothing

    it "allows occupied final tile (for attacks)" $ do
      let withEnemy = emptyGrid
            & ix (MkAxial 2 4) . tile_content ?~ Enemy defUnit
      findPath withEnemy (MkAxial 2 3) (MkAxial 2 4) `shouldBe` Just [MkAxial 2 4]

    it "returns Nothing when destination is off-grid" $
      findPath emptyGrid (MkAxial 0 0) (MkAxial 10 10) `shouldBe` Nothing

    it "returns Nothing when destination is Water" $ do
      let waterDest = emptyGrid & ix (MkAxial 2 4) . tile_terrain .~ Water
      findPath waterDest (MkAxial 2 3) (MkAxial 2 4) `shouldBe` Nothing
