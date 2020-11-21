{-# LANGUAGE TemplateHaskell #-}

module Grid(Grid(..), Tile(..), initialGrid, _r, _q) where

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
