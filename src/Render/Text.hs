{-# LANGUAGE TemplateHaskell #-}

module Render.Text(renderText) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Data.ByteString
import           Data.FileEmbed
import           Data.Foldable
import           Data.Int
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Vector.Storable   as S
import           Foreign.C.Types        (CInt)
import           Grid
import           Reflex
import           Reflex.SDL2
import qualified Render.Font            as Font
import           Render.Image
import           Render.Layer
import           SDL.Font
import           Text.Printf

renderText
  :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Font -> Color -> Point V2 CInt -> Text -> m ImageSettings
renderText font color position text = do
      r    <- ask
      textSurface <- Font.solid font color text
      fontHexSize <- fmap fromIntegral . uncurry V2 <$> Font.size font text
      textTexture <- createTextureFromSurface r textSurface -- I think textures are cleaned automatically
      freeSurface textSurface
      pure $ ImageSettings
          { _image_position   =  Rectangle
              (  position
              -  (_Point # fontHexSize `quotV2` V2 2 (-5))
              ) fontHexSize
          , _image_content   = textTexture
          }

quotV2 :: V2 CInt -> V2 CInt -> V2 CInt
quotV2 (V2 x y) (V2 x2 y2) = V2 (x `quot` x2) $ y `quot` y2
