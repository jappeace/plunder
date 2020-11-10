{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hexagon(hexagon ,
              HexagonSettings,
              defHex, hexagon_postion, hexagon_size, renderTile) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import qualified Data.Vector.Storable as S
import           Foreign.C.Types      (CInt)
import           Layer
import           Reflex
import           Reflex.SDL2
import Data.Word
import qualified Font
import Data.Foldable
import Data.Text(Text)
import qualified Data.Text as Text
import Grid
import Text.Printf

data HexagonSettings = HexagonSettings
  { _hexagon_postion :: Point V2 CInt
  , _hexagon_size    :: V2 CInt
  , _hexagon_label   :: Maybe Text
  }
makeLenses ''HexagonSettings

quotV2 :: V2 CInt -> V2 CInt -> V2 CInt
quotV2 (V2 x y) (V2 x2 y2) = V2 (x `quot` x2) $ y `quot` y2

_topLeft :: HexagonSettings -> Point V2 CInt
_topLeft settings = settings ^. hexagon_postion - (_Point # halveSize)
  where
    halveSize = view hexagon_size settings `quotV2` V2 2 2

defHex :: HexagonSettings
defHex = HexagonSettings
  { _hexagon_postion = _Point # V2 150 150
  , _hexagon_size    = V2 80 45
  , _hexagon_label   = Nothing
  }

-- | Calc the points to render, we are pointy top.
--  https://www.redblobgames.com/grids/hexagons/#basics
--
--                        top point
--   top left point                    top right point
--   bottom left point                 bottom right point
--                      bottom point
calcPoints :: HexagonSettings -> S.Vector (Point V2 CInt)
calcPoints settings = do
   S.fromList $ review _Point <$> [topPoint, topRightPoint, bottomRightPoint, bottomPoint, bottomLeftPoint, topLeftPoint, topPoint]
  where
    topPoint :: V2 CInt
    topPoint = position - V2 0 ((size ^. _y) `quot` 2)

    topRightPoint :: V2 CInt
    topRightPoint = topPoint + midleTransform

    bottomRightPoint :: V2 CInt
    bottomRightPoint = topRightPoint + V2 0 ribLength

    bottomPoint :: V2 CInt
    bottomPoint = bottomRightPoint - ((_y *~ (-1)) midleTransform)

    bottomLeftPoint :: V2 CInt
    bottomLeftPoint = bottomPoint - midleTransform

    topLeftPoint :: V2 CInt
    topLeftPoint = bottomLeftPoint - V2 0 ribLength

    ribLength :: CInt
    ribLength = floor $ (fromIntegral (size ^. _y) * twoThirds)

    twoThirds :: Double
    twoThirds = 2/3

    size :: V2 CInt
    size = settings ^. hexagon_size

    position :: V2 CInt
    position = settings ^. hexagon_postion . _Point

    midleTransform :: V2 CInt
    midleTransform = V2 ((size ^. _x) `quot` 2) ribLength

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

renderTile :: Tile -> HexagonSettings
renderTile tile = hexagon_postion .~ (_Point # V2 x y)
                $ hexagon_label ?~ (Text.pack $ printf "%i,%i" (tile ^. _q) $ (tile ^. _r))
                $ defHex
  where
    x :: CInt
    x = floor $ size_x *
      (sqrt3 * (fromIntegral $ tile ^. _q) + sqrt3 / 2.0 * (fromIntegral $ tile ^. _r))

    y :: CInt
    y = floor $ size_y * (3.0 / two * (fromIntegral $ tile ^. _r))

    size_x :: Double
    size_x = (fromIntegral $ defHex ^. hexagon_size . _x) * (1/ sqrt3)

    size_y :: Double
    size_y = (fromIntegral $ defHex ^. hexagon_size . _y) * (0.87)

    two :: Double
    two = 2.0

sqrt3 :: Double
sqrt3 = sqrt 3.0
