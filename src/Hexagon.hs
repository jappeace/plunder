{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hexagon(hexagon ,
              HexagonSettings,
              defHex, hexagon_postion, hexagon_size) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import qualified Data.Vector.Storable as S
import           Foreign.C.Types      (CInt)
import           Layer
import           Reflex
import           Reflex.SDL2

data HexagonSettings = HexagonSettings
  { _hexagon_postion :: V2 CInt
  , _hexagon_size    :: V2 CInt
  }
makeLenses ''HexagonSettings

defHex :: HexagonSettings
defHex = HexagonSettings
  { _hexagon_postion = V2 150 150
  , _hexagon_size = V2 80 45
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
    position = settings ^. hexagon_postion

    midleTransform :: V2 CInt
    midleTransform = V2 ((size ^. _x) `quot` 2) ribLength

-- TODO:
-- 1. label.
-- 2. grid.
-- 3. detect click.
hexagon :: (ReflexSDL2 t m, MonadReader Renderer m, DynamicWriter t [Layer m] m)
  =>  HexagonSettings -> m ()
hexagon settings = do
  r <- ask
  evPB         <- holdDyn () =<< getPostBuild
  commitLayer $ ffor evPB $ const $ do
    rendererDrawColor r $= V4 128 128 128 255
    drawLines r points
  pure ()

  liftIO $ print points
  where
    points = calcPoints settings
