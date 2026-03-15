module Main (main) where

import Test.Hspec
import qualified Test.HexagonSpec      as HexagonSpec
import qualified Test.IntegrationSpec  as IntegrationSpec
import qualified Test.LevelSpec        as LevelSpec
import qualified Test.PathfindingSpec  as PathfindingSpec
import qualified Test.StateSpec        as StateSpec

main :: IO ()
main = hspec $ do
  describe "Test.Hexagon"      HexagonSpec.spec
  describe "Test.Integration"  IntegrationSpec.spec
  describe "Test.Level"        LevelSpec.spec
  describe "Test.Pathfinding"  PathfindingSpec.spec
  describe "Test.State"        StateSpec.spec
