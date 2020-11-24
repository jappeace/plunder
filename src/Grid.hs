{-# LANGUAGE TemplateHaskell #-}

module Grid
  ( Grid(..)
  , Axial(..)
  , initialGrid
  , _r
  , _q
  , roundAxial
  , axialToPixel
  , pixelToAxial
  , hexSize
  , neigbours
  , tile_coordinate
  , tile_content
  )
where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as SMap
import           GHC.Generics    (Generic)
import           Reflex.SDL2
import           Foreign.C.Types      (CInt)

hexSize :: Int
hexSize = 80

type Grid = Map Axial Tile

-- 2. grid. (we'll use axial) https://www.redblobgames.com/grids/hexagons/#coordinates
data Axial = MkAxial
  { __q :: Int
  , __r :: Int
  } deriving (Eq, Ord, Show, Generic)


data TileContent = Player | Enemy

data Tile = MkTile
  { _tile_coordinate :: Axial
  , _tile_content    :: Maybe TileContent
  }
makeLenses 'MkAxial
makeLenses 'MkTile

-- level
initialGrid :: Grid
initialGrid = SMap.fromList $ do
  q <- size
  r <- size
  let coordinate = MkAxial q r
  pure $ (coordinate , MkTile coordinate Nothing)

size :: [Int]
size = [0 .. 6]


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
