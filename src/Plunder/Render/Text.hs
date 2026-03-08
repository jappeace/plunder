{-# LANGUAGE TemplateHaskell #-}

module Plunder.Render.Text(
  renderText
  , Style(..)
  , Align(..)
  ,  styleHorizontalAlignLens
  ,  styleColorLens
  ,  defaultStyle
  , TextSurface
  , allocateText
  , surfaceToSettings
  , textSurfaceSize
  ) where

import Plunder.Lens
import           Control.Lens
import           Control.Monad.Reader   (MonadReader (..))
import           Data.Text              (Text)
import           Foreign.C.Types        (CInt)
import           Reflex.SDL2
import qualified Plunder.Render.Font            as Font
import           Plunder.Render.Image
import           SDL.Font hiding (Style)

data Align = Start | Center

data Style = MkStyle
  { styleHorizontalAlign :: Align
  , styleColor :: Color
  }

defaultStyle :: Style
defaultStyle =  MkStyle
  { styleHorizontalAlign = Start
  , styleColor = (V4 128 128 128 255)
  }

data TextSurface = MkTextSurface
  { textSurfaceFontHexSize :: V2 CInt
  , textSurfaceTexture :: Texture
  , textSurfaceStyle :: Style
  }

-- | Create a 'TextSurface' from a font, style, and text string.
--   This only allocates the SDL texture; it does not draw anything on screen.
allocateText :: (ReflexSDL2 t m, MonadReader Renderer m) => Font -> Style -> Text -> m TextSurface
allocateText font style text = do
      r    <- ask
      textSurface <- Font.solid font color text
      fontHexSize <- fmap fromIntegral . uncurry V2 <$> Font.size font text
      textTexture <- createTextureFromSurface r textSurface -- I think textures are cleaned automatically
      freeSurface textSurface
      pure $ MkTextSurface
          { textSurfaceFontHexSize = fontHexSize
          , textSurfaceTexture = textTexture
          , textSurfaceStyle = style
          }
      where
       color = styleColor style

-- | Convert a 'TextSurface' and a screen position into 'ImageSettings'.
--   Pure conversion — does not draw anything; pass the result to 'image' to display.
surfaceToSettings :: TextSurface -> Point V2 CInt -> ImageSettings
surfaceToSettings surface position  =
      ImageSettings
          { _image_position   =  Rectangle
              (calcPosition (textSurfaceFontHexSize surface)) (textSurfaceFontHexSize surface)
          , _image_content   = textSurfaceTexture surface
          }
     where

       calcPosition fontHexSize = case styleHorizontalAlign $ textSurfaceStyle surface of
         Start -> position
         Center -> (  position
              -  (_Point # fontHexSize `quotV2` V2 2 (-5))
              )

textSurfaceSize :: TextSurface -> V2 CInt
textSurfaceSize = textSurfaceFontHexSize

-- | Allocate a text texture and return its 'ImageSettings'.
--
--   __This does NOT display anything on screen.__  To actually render the text,
--   pass the returned 'ImageSettings' to 'image' (which calls 'commitLayer').
--
--   For a convenience wrapper that renders /and/ displays in one step, see
--   @panelText@ in "Plunder.Render.ContextPanel".
renderText :: ReflexSDL2 t m
  => MonadReader Renderer m
  => Font -> Style -> Point V2 CInt -> Text -> m ImageSettings
renderText font style position text =
      flip surfaceToSettings position <$> allocateText font style text

quotV2 :: V2 CInt -> V2 CInt -> V2 CInt
quotV2 (V2 x y) (V2 x2 y2) = V2 (x `quot` x2) $ y `quot` y2

makePostfixLenses ''Style
