{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell #-}

module Hexagon(hexagon ,
              HexagonSettings,
              defHex, hexagon_postion, hexagon_size) where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import Layer
import Control.Lens
import Foreign.C.Types(CInt)
import qualified Data.Vector.Storable as S

data HexagonSettings = HexagonSettings
  { _hexagon_postion :: V2 CInt
  , _hexagon_size :: V2 CInt
  }
makeLenses ''HexagonSettings

defHex :: HexagonSettings
defHex = HexagonSettings
  { _hexagon_postion = V2 150 150
  , _hexagon_size = V2 100 50
  }

-- |                      top point
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
    ribLength = ((size ^. _y) `quot` 3)

    size :: V2 CInt
    size = settings ^. hexagon_size

    position :: V2 CInt
    position = settings ^. hexagon_postion

    midleTransform :: V2 CInt
    midleTransform = V2 ((size ^. _x) `quot` 2) ribLength


hexagon :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
  =>  HexagonSettings -> m ()
hexagon settings = do
  r <- ask
  rendererDrawColor r $= V4 100 150 75 255
  drawLines r $ calcPoints settings
  pure ()
