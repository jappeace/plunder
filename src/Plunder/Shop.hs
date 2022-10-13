{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plunder.Shop where

import Plunder.Lens
import Data.Word(Word64)
import           Control.Lens               hiding (elements)
import           Plunder.Combat
import           Test.QuickCheck
import           GHC.Generics               (Generic)
import Data.Text(Text, pack)
import Data.Set(Set)

data ShopItemType = ShopWeapon Weapon
                  | ShopUnit -- ^ friend for hire
                  | ShopHealthPotion -- ^ heal up
                  deriving (Show, Eq, Generic, Ord)

data Haul = MkHaul
  { haulItems :: Set ShopItem
  , haulNewMoney :: Word64
  }
  deriving Show

data ShopAction = MkBought Haul
                | MkExited
                deriving Show


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
    si_price :: Word64, si_type :: ShopItemType
  }
                  deriving (Show, Eq, Generic, Ord)

-- fixed amount of shop content for EZ rendering~
data ShopContent = MkShopContent
  { slot1 :: Maybe ShopItem
  , slot2 :: Maybe ShopItem
  , slot3 :: Maybe ShopItem
  }  deriving (Show, Eq, Generic)


makeLenses ''ShopContent
makePostfixLenses ''ShopItem
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
