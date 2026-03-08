{-# LANGUAGE RankNTypes #-}

-- | Abstraction over SDL 'Renderer' for testability.
--   Production code uses 'mkRenderFun' to partially apply the real renderer;
--   tests can supply a mock that captures render calls.
module Plunder.Render.RenderFun
  ( RenderFun(..)
  , mkRenderFun
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString        (ByteString)
import           Data.Int               (Int16)
import           Data.Word              (Word8)
import qualified Data.Vector.Storable   as S
import           Foreign.C.Types        (CInt)
import           Reflex.SDL2            (($=), Rectangle, Renderer, Surface,
                                         Texture, V2, V4, clear, copy,
                                         createTextureFromSurface, drawRect,
                                         fillRect, present,
                                         rendererDrawColor)
import           SDL.Primitive          (Color, fillPolygon, fillTriangle,
                                         polygon, thickLine)
import           SDL.Image              (decodeTexture)
import           Plunder.Render.Color   (setDrawColor)

data RenderFun = MkRenderFun
  { rf_fillRect             :: forall m. MonadIO m => Maybe (Rectangle CInt) -> m ()
  , rf_drawRect             :: forall m. MonadIO m => Maybe (Rectangle CInt) -> m ()
  , rf_setDrawColor         :: forall m. MonadIO m => Color -> m ()
  , rf_copy                 :: forall m. MonadIO m => Texture -> Maybe (Rectangle CInt)
                                                   -> Maybe (Rectangle CInt) -> m ()
  , rf_fillPolygon          :: forall m. MonadIO m => S.Vector Int16 -> S.Vector Int16
                                                   -> Color -> m ()
  , rf_polygon              :: forall m. MonadIO m => S.Vector Int16 -> S.Vector Int16
                                                   -> Color -> m ()
  , rf_thickLine            :: forall m. MonadIO m => V2 CInt -> V2 CInt -> CInt
                                                   -> Color -> m ()
  , rf_fillTriangle         :: forall m. MonadIO m => V2 CInt -> V2 CInt -> V2 CInt
                                                   -> Color -> m ()
  , rf_createTexture        :: forall m. MonadIO m => Surface -> m Texture
  , rf_decodeTexture        :: forall m. MonadIO m => ByteString -> m Texture
  , rf_clear                :: forall m. MonadIO m => m ()
  , rf_present              :: forall m. MonadIO m => m ()
  , rf_setRendererDrawColor :: forall m. MonadIO m => V4 Word8 -> m ()
  }

-- | Build a 'RenderFun' from a real SDL 'Renderer'.
mkRenderFun :: Renderer -> RenderFun
mkRenderFun r = MkRenderFun
  { rf_fillRect      = fillRect r
  , rf_drawRect      = drawRect r
  , rf_setDrawColor  = setDrawColor r
  , rf_copy          = copy r
  , rf_fillPolygon   = fillPolygon r
  , rf_polygon       = polygon r
  , rf_thickLine     = \a b w c -> thickLine r a b w c
  , rf_fillTriangle  = \a b c col -> fillTriangle r a b c col
  , rf_createTexture        = createTextureFromSurface r
  , rf_decodeTexture        = \bs -> decodeTexture r bs
  , rf_clear                = clear r
  , rf_present              = present r
  , rf_setRendererDrawColor = \c -> rendererDrawColor r $= c
  }
