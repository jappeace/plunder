{-# LANGUAGE TemplateHaskell #-}

module Render.Image(
            ImageSettings(..)
            , loadViking
            , loadEnemy
            , image
            , renderImage
            , rectangle_pos
            , rectangle_size
            , image_position
            , loadBlood
            , loadAxe
            , loadSword
            , loadBow
            , loadHouse
            , renderWeapon
            , burndedHouse
            ) where

import System.Random
import Control.Monad
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Data.ByteString        hiding (copy)
import           Data.FileEmbed
import           Foreign.C.Types        (CInt)
import           Grid
import           Render.Layer
import           Reflex
import           Reflex.SDL2
import           SDL.Image

loadAxe :: MonadIO m => MonadReader Renderer m => m Texture
loadAxe = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/axe.png")

loadSword :: MonadIO m => MonadReader Renderer m => m Texture
loadSword = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/sword.png")

loadBow :: MonadIO m => MonadReader Renderer m => m Texture
loadBow = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/bow.png")

loadViking :: MonadIO m => MonadReader Renderer m => m Texture
loadViking = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/viking.png")

loadBlood :: MonadIO m => MonadReader Renderer m => m Texture
loadBlood = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/blood.png")

-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Video-Renderer.html#v:surfaceBlit
-- https://hackage.haskell.org/package/sdl2-2.5.3.0/docs/SDL-Video-Renderer.html#v:createTextureFromSurface
burndedHouse :: MonadIO m => MonadReader Renderer m => m Texture
burndedHouse = do
  fire <- decode imgFireFile
  dimsFire <- surfaceDimensions fire
  house <- decode imgHouseFile
  dims <- surfaceDimensions house
  count <- liftIO $ randomRIO (8,20)
  replicateM count $ do
    rx <- liftIO $ randomRIO (-(dimsFire ^. _x), (dims ^. _x) - (dimsFire ^. _x))
    ry <- liftIO $ randomRIO (-(dimsFire ^. _y), (dims ^. _y) - (dimsFire ^. _y))
    void $ surfaceBlit fire Nothing house (Just $ P $ V2 rx ry)

  r1 <- ask
  createTextureFromSurface r1 house

imgFireFile :: ByteString
imgFireFile = $(embedFile "assets/img/fire.png")

loadHouse :: MonadIO m => MonadReader Renderer m => m Texture
loadHouse = flip decodeTexture imgHouseFile =<< ask

imgHouseFile :: ByteString
imgHouseFile = $(embedFile "assets/img/house.png")


loadEnemy :: MonadIO m => MonadReader Renderer m => m Texture
loadEnemy = flip decodeTexture imgFile =<< ask
  where
  imgFile :: ByteString
  imgFile = $(embedFile "assets/img/male_adventurer_idle.png")

data ImageSettings = ImageSettings
  { _image_position :: Rectangle CInt
  , _image_content :: Texture
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
        settings ^? image_position

renderWeapon :: ImageSettings -> ImageSettings
renderWeapon =
  (image_position . rectangle_pos . _x -~ 20)
  .
  (image_position . rectangle_pos . _y -~ 10)
  .
  (image_position . rectangle_size . _x -~ 20)
  .
  (image_position . rectangle_size . _y -~ 20)

renderImage :: Texture -> Axial -> ImageSettings
renderImage text coord = ImageSettings {
        _image_position  = Rectangle (axialToPixel coord) (V2 50 50)
      , _image_content  = text
      }

rectangle_pos :: Lens' (Rectangle a) (Point V2 a)
rectangle_pos = lens (\(Rectangle a _) -> a) (\(Rectangle _ b) a -> Rectangle a b)

rectangle_size :: Lens' (Rectangle a) (V2 a)
rectangle_size = lens (\(Rectangle _ b) -> b) (\(Rectangle a _) b -> Rectangle a b)
