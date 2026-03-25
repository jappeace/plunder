{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Plunder.Render.Hexagon
  ( hexagon
  , hexagonDyn
  , HexagonSettings
  , defHex
  , hexagon_position
  , hexagon_color
  , hexagon_is_filled
  , hexagon_label
  , renderHex
  , renderHexCam
  )
where

import           SDL.Font(Font)
import           Plunder.Render.Text
import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import           Plunder.Render.RenderFun (RenderFun(..))
import           Data.Foldable
import           Data.Int

import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector.Storable as S
import           Foreign.C.Types      (CInt)
import qualified Unwitch.Convert.Int as Int
import qualified Unwitch.Convert.CInt as CInt
import           Plunder.Grid
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           SDL.Primitive (Color)
import           Text.Printf

data HexagonSettings = HexagonSettings
  { _hexagon_position  :: Point V2 CInt
  , _hexagon_label     :: Maybe Text
  , _hexagon_color     :: Color
  , _hexagon_is_filled :: Bool
  , _hexagon_font      :: Font
  }
makeLenses ''HexagonSettings

defHex :: HexagonSettings
defHex = HexagonSettings { _hexagon_position  = _Point # V2 150 150
                         , _hexagon_label     = Nothing
                         , _hexagon_color     = V4 128 128 128 255
                         , _hexagon_is_filled = False
                         , _hexagon_font      = error "no hexagon font set" -- fixme, this is bad
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
  TopRightPoint    -> 330
  BottomRightPoint -> 30
  BottomPoint      -> 90
  BottomLeftPoint  -> 150
  TopLeftPoint     -> 210
  TopPoint         -> 270

-- https://www.redblobgames.com/grids/hexagons/#angles
pointyHexCorner :: Point V2 CInt -> CInt -> HexCorner -> Point V2 CInt
pointyHexCorner (P (V2 x y)) hexSize' corner =
  (P $ V2 (x + floor (CInt.toDouble hexSize' * cos rad))
          (y + floor (CInt.toDouble hexSize' * sin rad))
  )
 where
  -- | Degrees (30–330) always fit in Double; const 0 is unreachable.
  intToDouble :: Int -> Double
  intToDouble = either (const 0) id . Int.toDouble
  degree :: Double
  degree = intToDouble $ cornerToDegree corner
  rad :: Double
  rad = pi / 180 * (degree)

-- | Calc the points to render, we are pointy top.
--  https://www.redblobgames.com/grids/hexagons/#basics
calcPoints :: HexagonSettings -> (S.Vector Int16, S.Vector Int16)
calcPoints settings =
  ( S.fromList $ cintToInt16Clamp . view _x <$> points
  , S.fromList $ cintToInt16Clamp . view _y <$> points
  )
 where
  -- | Clamp pixel coordinates to Int16 bounds for SDL polygon rendering.
  cintToInt16Clamp :: CInt -> Int16
  cintToInt16Clamp c = case CInt.toInt16 c of
    Just i  -> i
    Nothing -> if CInt.toInt c > 0 then maxBound else minBound
  points :: [Point V2 CInt]
  points =  pointyHexCorner (settings ^. hexagon_position) hexSize <$> allCorners
  allCorners :: [HexCorner]
  allCorners = [minBound .. maxBound]

-- TODO:
-- 3. detect click. https://www.redblobgames.com/grids/hexagons/#pixel-to-hex
hexagon
  :: ReflexSDL2 t m
  => MonadReader RenderFun m
  => DynamicWriter t [Layer m] m => HexagonSettings -> m ()
hexagon settings = do
  MkRenderFun{rf_fillPolygon, rf_polygon} <- ask
  let polgyonF = if settings ^. hexagon_is_filled then rf_fillPolygon else rf_polygon
      (xPoints, yPoints) = calcPoints settings
  evPB <- holdDyn () =<< getPostBuild
  commitLayer $ ffor evPB $ const $ do
    polgyonF xPoints yPoints $ settings ^. hexagon_color
  for_ (settings ^. hexagon_label) $ \text -> do
      imageSettings <- renderText (settings ^. hexagon_font) (MkStyle
                                                                { styleHorizontalAlign = Center
                                                                , styleColor = settings ^. hexagon_color
                                                                }) (settings ^. hexagon_position) text
      image $ pure $ Just imageSettings
  pure ()

-- | Like 'hexagon' but takes dynamic settings, so the outline moves with the camera.
hexagonDyn
  :: ReflexSDL2 t m
  => MonadReader RenderFun m
  => DynamicWriter t [Layer m] m => Dynamic t HexagonSettings -> m ()
hexagonDyn settingsDyn = do
  MkRenderFun{rf_fillPolygon, rf_polygon} <- ask
  commitLayer $ ffor settingsDyn $ \settings -> do
    let polgyonF = if settings ^. hexagon_is_filled then rf_fillPolygon else rf_polygon
        (xPoints, yPoints) = calcPoints settings
    polgyonF xPoints yPoints $ settings ^. hexagon_color

-- https://www.redblobgames.com/grids/hexagons/#hex-to-pixel
renderHex :: Font -> Axial -> HexagonSettings
renderHex = renderHexCam (V2 0 0)

-- | Like 'renderHex' but with a camera pixel offset applied.
renderHexCam :: V2 CInt -> Font -> Axial -> HexagonSettings
renderHexCam cam font coord = hexagon_position .~ (axialToPixelCam cam coord)
                $ hexagon_label ?~ (Text.pack $ printf "%i,%i" (coord ^. _q) $ (coord ^. _r))
                $ hexagon_font .~ font
                $ defHex
