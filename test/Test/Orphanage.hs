{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphanage () where

import           Foreign.C.Types (CInt)
import           Game13          ()
import           Reflex.SDL2
import           Test.QuickCheck
import State
import Control.Lens
-- import Grid
-- import qualified Data.Map.Strict as SMap
-- import Control.Monad

instance Arbitrary (V2 CInt) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Point V2 CInt) where
  arbitrary = P <$> arbitrary
  shrink = genericShrink

noMoveSelf :: Move -> Bool
noMoveSelf x = (x ^. move_from) /= (x ^. move_to)

instance Arbitrary Move where
  arbitrary = suchThat (MkMove <$> arbitrary <*> arbitrary) noMoveSelf
  shrink = filter noMoveSelf . genericShrink


-- my quicheck too slow, try making it better by creating my own map
-- still slow
-- instance {-# OVERLAPPING  #-} Arbitrary Grid where
--   arbitrary = do
--     one <- replicateM 100 arbitrary
--     two <- arbitrary
--     three <- arbitrary
--     four <- arbitrary
--     let
--       list :: Grid
--       list = SMap.fromList $ join $ [join one,two,three,four]
--     pure list
--   shrink = shrink1
