{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hexagon(hexagon ,
              HexagonSettings,
              defHex, hexagon_postion, renderTile, detectTile, detectPoint
              ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import           Data.Foldable
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector.Storable as S
import           Data.Word
import qualified Font
import           Foreign.C.Types      (CInt)
import           Grid
import           Layer
import           Reflex
import           Reflex.SDL2
import           Text.Printf

data HexagonSettings = HexagonSettings
  { _hexagon_postion :: Point V2 CInt
  , _hexagon_label   :: Maybe Text
  }
makeLenses ''HexagonSettings

quotV2 :: V2 CInt -> V2 CInt -> V2 CInt
quotV2 (V2 x y) (V2 x2 y2) = V2 (x `quot` x2) $ y `quot` y2

size :: Int
size = 80

defHex :: HexagonSettings
defHex = HexagonSettings
  { _hexagon_postion = _Point # V2 150 150
  , _hexagon_label   = Nothing
  }

-- | The corners of a hexagon labeled.
--
--                        top point
--   top left point                    top right point
--   bottom left point                 bottom right point
--                      bottom point
data HexCorner = TopRightPoint
              | BottomRightPoint
              | BottomPoint
              | BottomLeftPoint
              | TopLeftPoint
              | TopPoint
              deriving (Enum, Bounded)

cornerToDegree :: HexCorner -> Int
cornerToDegree = \case
              TopRightPoint     -> 330
              BottomRightPoint  -> 30
              BottomPoint       -> 90
              BottomLeftPoint   -> 150
              TopLeftPoint      -> 210
              TopPoint          -> 270

-- https://www.redblobgames.com/grids/hexagons/#angles
pointyHexCorner :: Point V2 CInt -> Int -> HexCorner -> Point V2 CInt
pointyHexCorner (P (V2 x y)) size corner =
  (P $ V2 (x + (floor $ fromIntegral size * cos rad)) (y + (floor $ fromIntegral size * sin rad)))
    where
      degree :: Double
      degree = fromIntegral $ cornerToDegree corner
      rad :: Double
      rad = pi / 180 * (degree)

-- | Calc the points to render, we are pointy top.
--  https://www.redblobgames.com/grids/hexagons/#basics
calcPoints :: HexagonSettings -> S.Vector (Point V2 CInt)
calcPoints settings = do
   S.fromList $ pointyHexCorner (settings ^. hexagon_postion) size <$> allCorners
  where
    allCorners :: [HexCorner]
    allCorners = [minBound..maxBound]

someColor :: V4 Word8
someColor = V4 128 128 128 255

-- TODO:
-- 3. detect click. https://www.redblobgames.com/grids/hexagons/#pixel-to-hex
hexagon :: ReflexSDL2 t m
      => MonadReader Renderer m => DynamicWriter t [Layer m] m
  =>  HexagonSettings -> m ()
hexagon settings = do
  r <- ask
  font <- Font.defaultFont
  evPB         <- holdDyn () =<< getPostBuild
  commitLayer $ ffor evPB $ const $ do
    rendererDrawColor r $= someColor
    drawLines r points
    for_ (settings ^. hexagon_label) $ \text -> do
      textSurface <- Font.solid font someColor text
      fontSize <- fmap fromIntegral . uncurry V2 <$> Font.size font text
      textTexture <- createTextureFromSurface r textSurface -- I think textures are cleaned automatically
      freeSurface textSurface
      copy r textTexture Nothing $ Just $ Rectangle (settings ^. hexagon_postion - (_Point # fontSize `quotV2` V2 2 (-5))) $ fontSize
  pure ()
  where
    points = calcPoints settings

-- https://www.redblobgames.com/grids/hexagons/#hex-to-pixel
renderTile :: Tile -> HexagonSettings
renderTile tile = hexagon_postion .~ (detectPoint tile)
                $ hexagon_label ?~ (Text.pack $ printf "%i,%i" (tile ^. _q) $ (tile ^. _r))
                $ defHex

detectPoint :: Tile -> Point V2 CInt
detectPoint tile = (P $ V2 x y)
  where
    x :: CInt
    x = floor $ fromIntegral size *
      (sqrt3 * (fromIntegral $ tile ^. _q) + sqrt3 / 2.0 * (fromIntegral $ tile ^. _r))

    y :: CInt
    y = floor $ fromIntegral size * (3.0 / two * (fromIntegral $ tile ^. _r))

    two :: Double
    two = 2.0

-- https://www.redblobgames.com/grids/hexagons/#pixel-to-hex
detectTile :: Point V2 CInt -> Tile
detectTile (P (V2 x y)) = Tile q r
  where
    -- TODO Implement size properly, this math is crazy
    q :: Int
    q = floor $ (sqrt(3)/3 * fromIntegral x - 1/3 * fromIntegral y) / fromIntegral size
    r :: Int
    r = floor $ (2.0/3 * fromIntegral y) / fromIntegral size

sqrt3 :: Double
sqrt3 = sqrt 3.0
