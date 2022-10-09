{-# LANGUAGE TemplateHaskell #-}

-- | Color support
module Plunder.Render.Color(setDrawColor) where

import           Reflex.SDL2
import           Control.Lens
import           Control.Monad
import           SDL.Internal.Types     (Renderer (..))
import           SDL.Primitive          (Color)
import           SDL.Raw.Video          (setRenderDrawColor)

-- | it's confusing because the upstream uses a type alias
setDrawColor :: MonadIO m => Renderer -> Color -> m ()
setDrawColor (Renderer ptr) color =
    void $ setRenderDrawColor ptr (color ^. _x) (color ^. _y) (color ^. _z) (color ^. _w)
