{-# LANGUAGE TemplateHaskell       #-}

module Image(ImageSettings(..), loadViking, image) where

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
    => ImageSettings -> m ()
image settings = do
  evPB <- holdDyn () =<< getPostBuild
  renderer    <- ask
  commitLayer $ ffor evPB $ const $ do
      copy renderer (settings ^. image_content) Nothing $
        settings ^? image_postion
