{-# LANGUAGE TemplateHaskell #-}

module Grid(Grid(..), Tile(..), initialGrid, _r, _q, roundTile ) where

import           Control.Lens
import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as SMap
import           GHC.Generics    (Generic)


newtype Grid = Grid { unGrid :: Map Tile Tile }

-- 2. grid. (we'll use axial) https://www.redblobgames.com/grids/hexagons/#coordinates
data Tile = Tile
  { __q :: Int
  , __r :: Int
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''Tile

-- level
initialGrid :: Grid
initialGrid = Grid $ SMap.fromList $ do
  q <- size
  r <- size
  pure $ (Tile q r, Tile q r)

size :: [Int]
size = [0..6]


-- https://www.redblobgames.com/grids/hexagons/#rounding
-- https://www.redblobgames.com/grids/hexagons/#conversions
roundTile :: Double -> Double -> Tile
roundTile q r =
  Tile
  { __q = if q_override then -ry-rz else rx
  , __r = if r_override then -rx-ry else rz
  }
  where
    q_override :: Bool
    q_override = x_diff > y_diff && x_diff > z_diff

    r_override :: Bool
    r_override = (not q_override) && y_diff <= z_diff

    x_diff = abs $ fromIntegral rx - x
    y_diff = abs $ fromIntegral ry - y
    z_diff = abs $ fromIntegral rz - z

    rx = round x
    ry = round y
    rz = round z

    x = q
    z = r
    y = -x-z
