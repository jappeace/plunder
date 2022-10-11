{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plunder.Shop where

import           Control.Lens               hiding (elements)
import           Plunder.Combat
import           Test.QuickCheck
import           GHC.Generics               (Generic)
import Data.Text(Text, pack)

data ShopItemType = ShopWeapon Weapon
                  | ShopUnit -- ^ friend for hire
                  | ShopHealthPotion -- ^ heal up
                  deriving (Show, Eq, Generic)

-- | user facing description
itemTypeDescription :: ShopItemType -> Text
itemTypeDescription = \case
  ShopWeapon weapon -> weaponDescription weapon
  ShopUnit -> "friend"
  ShopHealthPotion -> "health potion"

itemDescription :: ShopItem  -> Text
itemDescription MkShopItem {..} =
          "$ " <> tshow si_price <> " - " <> itemTypeDescription si_type

tshow :: Show a => a -> Text
tshow = pack . show

data ShopItem = MkShopItem {
    si_price :: Int, si_type :: ShopItemType
  }
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
