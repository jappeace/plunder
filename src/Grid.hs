{-# LANGUAGE TemplateHaskell #-}

module Grid
  ( Grid(..)
  , Tile(..)
  , initialGrid
  , _r
  , _q
  , roundTile
  , tileToPixel
  , pixelToTile
  , hexSize
  , neigbours
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
size = [0 .. 6]


-- https://www.redblobgames.com/grids/hexagons/#rounding
-- https://www.redblobgames.com/grids/hexagons/#conversions
roundTile :: Double -> Double -> Tile
roundTile q r = Tile { __q = if q_override then -ry - rz else rx
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
tileToPixel :: Tile -> Point V2 CInt
tileToPixel tile = (P $ V2 x y)
 where
  x :: CInt
  x =
    floor
      $ fromIntegral hexSize
      * ( (sqrt3 * (fromIntegral $ tile ^. _q))
        + (sqrt3 / 2.0 * (fromIntegral $ tile ^. _r))
        )
  y :: CInt
  y = floor $ fromIntegral hexSize * (3.0 / two * (fromIntegral $ tile ^. _r))

-- https://www.redblobgames.com/grids/hexagons/#pixel-to-hex
pixelToTile :: Point V2 CInt -> Tile
pixelToTile (P vec) = roundTile q r
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

neigbours :: Tile -> [Tile]
neigbours parent =
       filter (\x -> not $ has (at x) $ unGrid initialGrid) $ neighList <*> [parent]
  where
    neighList :: [Tile -> Tile]
    neighList = [ (_q +~ 1)
                , (_r +~ 1)
                , (_q -~ 1) . (_r +~ 1)
                , (_q -~ 1)
                , (_r -~ 1)
                , (_q +~ 1) . (_r -~ 1)
                ]
