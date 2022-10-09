{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphanage () where

import           Foreign.C.Types (CInt)
import           Plunder          ()
import           Reflex.SDL2
import           Test.QuickCheck
import Plunder.State
import Control.Lens

instance Arbitrary (V2 CInt) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Point V2 CInt) where
  arbitrary = P <$> arbitrary
  shrink = genericShrink

noMoveSelf :: Move -> Bool
noMoveSelf x =
  (x ^. move_from) /= (x ^. move_to)


instance Arbitrary Move where
  arbitrary = suchThat (MkMove <$> arbitrary <*> arbitrary  <*> arbitrary) noMoveSelf
  shrink = filter noMoveSelf . genericShrink
