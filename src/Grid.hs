{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Grid(Grid(..), Tile(..), initialGrid, _r, _q) where

import qualified Data.Map.Strict as SMap
import Data.Map.Strict(Map)
import           Control.Lens
import Control.Monad

newtype Grid = Grid { unGrid :: Map Tile Tile }

-- 2. grid. (we'll use axial) https://www.redblobgames.com/grids/hexagons/#coordinates
data Tile = Tile
  { __q :: Int
  , __r :: Int
  } deriving (Eq, Ord)

makeLenses ''Tile

-- level
initialGrid :: Grid
initialGrid = Grid $ SMap.fromList $ do
  q <- size
  r <- size
  pure $ (Tile q r, Tile q r)

size :: [Int]
size = [0..6]

