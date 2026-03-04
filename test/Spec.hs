module Main (main) where

import Test.Hspec
import qualified Test.HexagonSpec as HexagonSpec
import qualified Test.StateSpec   as StateSpec

main :: IO ()
main = hspec $ do
  describe "Test.Hexagon" HexagonSpec.spec
  describe "Test.State"   StateSpec.spec
