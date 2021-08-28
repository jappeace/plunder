{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , Health
  , unit_hp
  , isDead
  , isTargetDead
  , Result(..)
  , res_left_unit
  , res_right_unit
  )
where

import           Control.Lens               hiding (elements)
import           Control.Monad.Random.Class
import           GHC.Generics               (Generic)
import           Test.QuickCheck hiding (Result)

-- | Normally a weapon  does between 1 and 3 damage.
--   for example spear vs spead, roll a dice, that's your damage.
--   however if a spear goes against a bow, the spear is innefective.
--   and the bow is super effective.
--   this means the bow does 2x damage, and the spear only 0.5x
--
--   say the spear rolls 3 and the bow rolls 2, the bow does 4 and the spear does 2 (rounded up).
data Weapon = Sword -- rock
            | Bow   -- paper
            | Axe -- scissor
            deriving (Show, Eq, Generic, Bounded,  Enum)

data Damage = NoEffect
            | Smol'l -- smol'l
            | Medium
            | Bigly

beats :: Weapon -> Weapon
beats Sword = Axe
beats Bow   = Sword
beats Axe   = Bow



getDmg ::
  Maybe Weapon -- ^ attacker
  -> Maybe Weapon -- ^ defender
  -> Damage -- ^ defenders' damage
getDmg Nothing _ = NoEffect
getDmg _ Nothing = Bigly
getDmg (Just a) (Just b) = if
            | beats a == b -> Bigly
            | beats b == a -> Smol'l
            | True         -> Medium

type Health = Int -- TODO change to something better

isDead :: Health -> Bool
isDead x = x <= 0

data Unit = MkUnit
  { _unit_hp     :: Health
  , _unit_weapon :: Maybe Weapon
  } deriving (Show, Eq, Generic)

defUnit :: Unit
defUnit = MkUnit
  { _unit_hp = 10
  , _unit_weapon = Nothing
  }
makeLenses ''Unit
makePrisms ''Weapon

applyDamage :: Int -> Maybe Weapon -> Maybe Weapon -> Health
applyDamage rng applying defending=
  case getDmg applying defending of
    NoEffect -> 0
    Smol'l   -> quot rng 2
    Medium   -> rng
    Bigly    -> rng * 2

data Result = MkResult
  { _res_left_unit :: Unit -- ^ attacking unit
  , _res_right_unit :: Unit -- ^ defending unit
  }
makeLenses ''Result

isTargetDead :: Result -> Bool
isTargetDead x = x ^. res_right_unit . unit_hp . to isDead

resolveCombat :: MonadRandom m => Unit -> Unit -> m Result
resolveCombat leftunit rightunit = do
  -- damage left one,
  rng <- getRandomR (1,3)
  let dmgToRight = applyDamage rng (leftunit ^. unit_weapon) (rightunit ^. unit_weapon)

  rng2 <- getRandomR (1,3)
  let dmgToLeft = applyDamage rng2 (rightunit ^. unit_weapon) (leftunit ^. unit_weapon)

  pure $ MkResult
    { _res_left_unit  = unit_hp -~ dmgToLeft $ leftunit
    , _res_right_unit = unit_hp -~ dmgToRight $ rightunit
    }

instance Arbitrary Weapon where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Unit where
  arbitrary = do
    _unit_hp <- arbitrary
    _unit_weapon <- arbitrary
    pure $ MkUnit {..}
  shrink = genericShrink
