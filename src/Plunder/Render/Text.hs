{-# LANGUAGE TemplateHaskell #-}

module Plunder.Render.Text(
  renderText
  , Style(..)
  , Align(..)
  ,  styleHorizontalAlignLens
  ,  styleColorLens
  ,  defaultStyle
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

renderText :: ReflexSDL2 t m
  => MonadReader Renderer m
  => Font -> Style -> Point V2 CInt -> Text -> m ImageSettings
renderText font style position text = do
      r    <- ask
      textSurface <- Font.solid font color text
      fontHexSize <- fmap fromIntegral . uncurry V2 <$> Font.size font text
      textTexture <- createTextureFromSurface r textSurface -- I think textures are cleaned automatically
      freeSurface textSurface
      pure $ ImageSettings
          { _image_position   =  Rectangle
              (calcPosition fontHexSize) fontHexSize
          , _image_content   = textTexture
          }


     where
       color = styleColor style

       calcPosition fontHexSize = case styleHorizontalAlign style of
         Start -> position
         Center -> (  position
              -  (_Point # fontHexSize `quotV2` V2 2 (-5))
              )

quotV2 :: V2 CInt -> V2 CInt -> V2 CInt
quotV2 (V2 x y) (V2 x2 y2) = V2 (x `quot` x2) $ y `quot` y2

makePostfixLenses ''Style
