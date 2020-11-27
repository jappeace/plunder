{-# LANGUAGE TemplateHaskell       #-}

module Image(ImageSettings(..)
            , loadViking
            , image
            , renderImage
            , rectangle_pos
            , rectangle_size
            , image_postion
            ) where

import Grid
import           Reflex
import Data.ByteString hiding (copy)
import Control.Monad.IO.Class
import Data.FileEmbed
import           Layer
import           Foreign.C.Types      (CInt)
import           Control.Monad.Reader (MonadReader (..))
import           Reflex.SDL2
import SDL.Image
import           Control.Lens

loadViking :: MonadIO m => MonadReader Renderer m => m Texture
loadViking = flip decodeTexture vikingFile =<< ask
  where
  vikingFile :: ByteString
  vikingFile = $(embedFile "assets/img/viking.png")

data ImageSettings = ImageSettings
  { _image_postion   :: Rectangle CInt
  , _image_content   :: Texture
  }
makeLenses ''ImageSettings

image :: ReflexSDL2 t m
    => MonadReader Renderer m
    => DynamicWriter t [Layer m] m
    => Dynamic t (Maybe ImageSettings) -> m ()
image settingsDyn = do
  renderer    <- ask
  commitLayer $ ffor settingsDyn $ \msettings ->
    flip (maybe (pure ())) msettings $ \settings -> do
      copy renderer (settings ^. image_content) Nothing $
        settings ^? image_postion

renderImage :: Texture -> Axial -> ImageSettings
renderImage text coord = ImageSettings {
        _image_postion  = Rectangle (axialToPixel coord) (V2 50 50)
      , _image_content  = text
      }

rectangle_pos :: Lens' (Rectangle a) (Point V2 a)
rectangle_pos = lens (\(Rectangle a _) -> a) (\(Rectangle _ b) a -> Rectangle a b)

rectangle_size :: Lens' (Rectangle a) (V2 a)
rectangle_size = lens (\(Rectangle _ b) -> b) (\(Rectangle a _) b -> Rectangle a b)
