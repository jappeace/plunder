{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphanage () where

import           Foreign.C.Types (CInt)
import           Game13          ()
import           Grid
import           Reflex.SDL2
import           Test.QuickCheck

instance Arbitrary Tile where
  arbitrary = Tile <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (V2 CInt) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Point V2 CInt) where
  arbitrary = P <$> arbitrary
  shrink = genericShrink
