{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Combat
  (Weapon(..)
  , Unit(..)
  , defUnit
  , resolveCombat
  , unit_weapon
  , _Axe
  , _Bow
  , _Sword
  )
where

import Data.Singletons.Prelude.Bool
import           Control.Lens hiding (elements)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as SMap
import           GHC.Generics    (Generic)
import           Reflex.SDL2
import           Foreign.C.Types      (CInt)
import qualified Test.QuickCheck as Q
import Control.Monad.Random.Class

-- | Normally a weapon  does between 1 and 3 damage.
--   for example spear vs spead, roll a dice, that's your damage.
--   however if a spear goes against a bow, the spear is innefective.
--   and the bow is super effective.
--   this means the bow does 2x damage, and the spear only 0.5x
--
--   say the spear rolls 3 and the bow rolls 2, the bow does 4 and the spear does 2 (rounded up).
data Weapon = Sword -- rock
            | Bow   -- paper
            | Axe -- siscor
            deriving (Show, Eq, Generic, Bounded,  Enum)

data Damage = NoEffect
            | Small
            | Medium
            | Large

type family Beats ( a :: Weapon) where
     Beats 'Sword = 'Axe
     Beats 'Bow = 'Sword
     Beats 'Axe = 'Bow

type family GetDmg ( a :: Maybe Weapon ) (b :: Maybe Weapon) (c :: Damage) where
  GetDmg 'Nothing b = 'NoEffect
  GetDmg a 'Nothing = 'Large
  GetDmg (Just a) (Just b) =  GetWeaponDmg a b

type family GetWeaponDmg (a :: Weapon) (b :: Weapon) (c :: Damage) where
  GetWeaponDmg a b = If (Beats a :~: b) -- if a beats b
                      'Large
                     ( -- else
                        If (Beats b :~: a)
                       'Small -- small damage, eg your attacking something that's effective against you
                       'Medium
                     )

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
  (Nothing, _) -> 0
  (_, Nothing) -> supereffective
  (Just Sword, Just Bow) -> innefective
  (Just Sword, Just Axe) -> supereffective -- it's just an abstraction
  (Just Sword, Just Sword) -> rng
  (Just Bow, Just Bow) -> rng
  (Just Bow, Just Axe) -> innefective
  (Just Bow, Just Sword) -> supereffective
  (Just Axe, Just Bow) -> supereffective
  (Just Axe, Just Axe) -> rng
  (Just Axe, Just Sword) -> innefective
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

instance Q.Arbitrary Weapon where
  arbitrary = elements [minBound .. maxBound]

instance Q.Arbitrary Unit where
  arbitrary = do
    _unit_hp <- arbitrary
    _unit_weapon <- arbitrary
    pure $ MkUnit {..}
  shrink = genericShrink
