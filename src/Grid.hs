{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Grid
  ( Grid
  , Axial(..)
  , initialGrid
  , _r
  , _q
  , roundAxial
  , axialToPixel
  , pixelToAxial
  , hexSize
  , neigbours
  , tile_axial
  , tile_content
  , tile_background
  , TileContent(..)
  , Background(..)
  , Tile
  , _Player
  , _Enemy
  , _Blood
  , contentFold
  , mkGrid
  , defUnit
  )
where

import           Control.Lens hiding (elements)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as SMap
import           GHC.Generics    (Generic)
import           Reflex.SDL2
import           Foreign.C.Types      (CInt)
import           Test.QuickCheck

hexSize :: Int
hexSize = 80

type Grid = Map Axial Tile

-- 2. grid. (we'll use axial) https://www.redblobgames.com/grids/hexagons/#coordinates
data Axial = MkAxial
  { __q :: Int
  , __r :: Int
  } deriving (Eq, Ord, Show, Generic)

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

data TileContent = Player { _tc_unit :: Unit} | Enemy { _tc_unit :: Unit }
  deriving (Show, Generic, Eq)

data Background = Blood
  deriving (Show, Generic)

data Tile = MkTile
  { _tile_coordinate :: Axial
  , _tile_content    :: Maybe TileContent
  , _tile_background :: Maybe Background
  } deriving (Show, Generic)


makeLenses 'MkAxial
makeLenses 'MkTile
makeLenses ''Unit
makeLenses ''TileContent
makePrisms ''TileContent
makePrisms ''Background
makePrisms ''Weapon

-- | A read only coordinate lens for 'Tile',
--   within the module we can set but outside we can only read so it's
--   always the right coordinate in the tile.
tile_axial :: Getter Tile Axial
tile_axial = tile_coordinate

-- level
initialGrid :: Grid
initialGrid = mkGrid 0 6

mkGrid :: Int -> Int -> Grid
mkGrid begin end =
  SMap.fromList $ do
  q <- size
  r <- size
  let coordinate = MkAxial q r
  pure $ (coordinate , MkTile coordinate Nothing Nothing)
  where
    size :: [Int]
    size = [begin .. end]


-- https://www.redblobgames.com/grids/hexagons/#rounding
-- https://www.redblobgames.com/grids/hexagons/#conversions
roundAxial :: Double -> Double -> Axial
roundAxial q r = MkAxial
  { __q = if q_override then -ry - rz else rx
  , __r = if r_override then -rx - ry else rz
  }
 where
  q_override :: Bool
  q_override = x_diff > y_diff && x_diff > z_diff

  r_override :: Bool
  r_override = (not q_override) && y_diff <= z_diff

  x_diff     = abs $ fromIntegral rx - x
  y_diff     = abs $ fromIntegral ry - y
  z_diff     = abs $ fromIntegral rz - z

  rx         = round x
  ry         = round y
  rz         = round z

  x          = q
  z          = r
  y          = -x - z


-- https://www.redblobgames.com/grids/hexagons/#hex-to-pixel
axialToPixel :: Axial -> Point V2 CInt
axialToPixel coord = (P $ V2 x y)
 where
  x :: CInt
  x =
    floor
      $ fromIntegral hexSize
      * ( (sqrt3 * (fromIntegral $ coord ^. _q))
        + (sqrt3 / 2.0 * (fromIntegral $ coord ^. _r))
        )
  y :: CInt
  y = floor $ fromIntegral hexSize * (3.0 / two * (fromIntegral $ coord ^. _r))

-- https://www.redblobgames.com/grids/hexagons/#pixel-to-hex
pixelToAxial :: Point V2 CInt -> Axial
pixelToAxial (P vec) = roundAxial q r
 where
    -- TODO Implement hexSize properly, this math is crazy
  q :: Double
  q = ((sqrt3 / 3) * fromIntegral x - (1 / 3) * fromIntegral y)
    / fromIntegral hexSize
  r :: Double
  r = ((two / 3) * fromIntegral y) / fromIntegral hexSize
  y = vec ^. _y
  x = vec ^. _x

sqrt3 :: Double
sqrt3 = sqrt 3.0

two :: Double
two = 2.0 -- no better descriptive name in the universe, magick numbers DIE!

neigbours :: Axial -> [Axial]
neigbours parent = filter (\x -> SMap.member x initialGrid)
                 $ neighList <*> [parent]
  where
    neighList :: [Axial -> Axial]
    neighList = [ _q +~ 1
                , _r +~ 1
                , (_q -~ 1) . (_r +~ 1)
                , _q -~ 1
                , _r -~ 1
                , (_q +~ 1) . (_r -~ 1)
                ]

-- this can be a traversal according to type system, but
-- if we invalidate the target of the predicate (tile_content) it's invalid.
-- so we make it a fold untill we need it be a traversal.
contentFold :: Fold Grid Tile
contentFold = traversed . filtered (has (tile_content . _Just))

-- test cruft, don't want to expose these constructors
instance Arbitrary Axial where
  arbitrary = MkAxial <$> choose (0,6) <*> choose (0,6)
  shrink = genericShrink

instance Arbitrary Tile where
  arbitrary = MkTile <$> arbitrary <*>
    frequency [(10, Just <$> arbitrary ), (5, pure Nothing)]
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary TileContent where
  arbitrary = arbitrary
  shrink = genericShrink


instance Arbitrary Background where
  arbitrary = pure Blood
  shrink = genericShrink

instance Arbitrary Weapon where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Unit where
  arbitrary = do
    _unit_hp <- arbitrary
    _unit_weapon <- arbitrary
    pure $ MkUnit {..}
  shrink = genericShrink
