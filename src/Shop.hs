{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Shop where

import           Control.Lens               hiding (elements)
import           Combat
import           Test.QuickCheck
import           GHC.Generics               (Generic)

data ShopItemType = ShopWeapon Weapon
                  | ShopUnit -- ^ friend for hire
                  | ShopHealthPotion -- ^ heal up
                  deriving (Show, Eq, Generic)

data ShopItem = MkShopItem { si_price :: Int, si_type :: ShopItemType }
                  deriving (Show, Eq, Generic)

-- fixed amount of shop content for EZ rendering~
data ShopContent = MkShopContent
  { slot1 :: Maybe ShopItem
  , slot2 :: Maybe ShopItem
  , slot3 :: Maybe ShopItem
  }  deriving (Show, Eq, Generic)


makeLenses ''ShopContent
makeLenses ''ShopItem
makePrisms ''ShopItemType

instance Arbitrary ShopItemType where
  arbitrary = elements =<< sequence [ ShopWeapon <$> arbitrary
                       , pure ShopUnit
                       , pure ShopHealthPotion
                       ]
  shrink = genericShrink

instance Arbitrary ShopContent where
  arbitrary = MkShopContent <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ShopItem where
  arbitrary = MkShopItem <$> arbitrary <*> arbitrary
  shrink = genericShrink
