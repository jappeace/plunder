{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Combat
  (Weapon(..)
  , Unit(..)
  , defUnit
  , resolveCombat
  )
where

import           Control.Lens hiding (elements)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as SMap
import           GHC.Generics    (Generic)
import           Reflex.SDL2
import           Foreign.C.Types      (CInt)
import           Test.QuickCheck
import Control.Monad.Random.Class

-- | Normally a weapon  does between 1 and 3 damage.
--   for example spear vs spead, roll a dice, that's your damage.
--   however if a spear goes against a bow, the spear is innefective.
--   and the bow is super effective.
--   this means the bow does 2x damage, and the spear only 0.5x
--
--   say the spear rolls 3 and the bow rolls 2, the bow does 4 and the spear does 2 (rounded up).
data Weapon = Spear -- rock
            | Bow   -- paper
            | Horse -- siscor
            deriving (Show, Eq, Generic, Bounded,  Enum)

data Unit = MkUnit
  { _unit_hp :: Int
  , _unit_weapon :: Maybe Weapon
  } deriving (Show, Eq, Generic)

defUnit :: Unit
defUnit = MkUnit
  { _unit_hp = 10
  , _unit_weapon = Nothing
  }
makeLenses ''Unit
makePrisms ''Weapon

applyDamage :: Int -> Maybe Weapon -> Maybe Weapon -> Int
applyDamage rng applying definding = case (applying, definding) of
  (Nothing, Nothing) -> 0
  (_, Nothing) -> supereffective
  (Just Spear, Just Bow) -> innefective
  (Just Spear, Just Horse) -> supereffective -- it's just an abstraction
  (Just Spear, Just Spear) -> rng
  (Just Bow, Just Bow) -> rng
  (Just Bow, Just Horse) -> innefective
  (Just Bow, Just Spear) -> supereffective
  (Just Horse, Just Bow) -> supereffective
  (Just Horse, Just Horse) -> rng
  (Just Horse, Just Spear) -> innefective
  where
    supereffective :: Int
    supereffective = rng * 2
    innefective :: Int
    innefective = quot rng 2

resolveCombat :: MonadRandom m => Unit -> Unit -> m (Unit, Unit)
resolveCombat leftunit rightunit = do
  -- damage left one,
  rng <- getRandom
  let dmgToRight = applyDamage rng (leftunit ^. unit_weapon) (rightunit ^. unit_weapon)

  rng2 <- getRandom
  let dmgToLeft = applyDamage rng2 (rightunit ^. unit_weapon) (leftunit ^. unit_weapon)

  pure (unit_hp -~ dmgToLeft $ leftunit, unit_hp -~ dmgToRight $ rightunit)

instance Arbitrary Weapon where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Unit where
  arbitrary = do
    _unit_hp <- arbitrary
    _unit_weapon <- arbitrary
    pure $ MkUnit {..}
  shrink = genericShrink
