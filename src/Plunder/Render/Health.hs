
module Plunder.Render.Health(healthBar) where

import qualified Plunder.Combat as Combat
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Plunder.Render.RenderFun (RenderFun(..))
import           Data.Maybe
import           Foreign.C.Types        (CInt)
import           Plunder.Grid
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Layer
import SDL.Primitive(Color)

healthBar :: DynamicWriter t [Layer m] m
        => ReflexSDL2 t m
        => MonadReader RenderFun m
        => Dynamic t (V2 CInt) -> Dynamic t Tile -> m ()
healthBar cameraDyn tileDyn = do
  rf <- ask
  commitLayer $ healthBar' rf <$> cameraDyn <*> tileDyn

barHeight :: Num a => a
barHeight = 6

maxHealth :: Combat.Health
maxHealth = Combat.maxHealth

pixelsPerHealth :: Num a => a
pixelsPerHealth = 12

bgColor :: Color
bgColor = V4 40 40 40 220

fillColor :: Color
fillColor = V4 80 200 80 255

borderColor :: Color
borderColor = V4 0 0 0 255

healthBar' :: MonadIO m
        => RenderFun -> V2 CInt -> Tile -> m ()
healthBar' rf cam tile = unless isDead $ do
    -- dark background (full max-health width)
    rf_setDrawColor rf bgColor
    rf_fillRect rf $ Just bgRect
    -- green health fill
    rf_setDrawColor rf fillColor
    rf_fillRect rf $ Just fillRect'
    -- black border on top
    rf_setDrawColor rf borderColor
    rf_drawRect rf $ Just bgRect
    where
      isDead :: Bool
      isDead = fromMaybe True $ do
        hp' <- tile ^? tile_content . _Just . tc_unit . Combat.unit_hp
        pure $ Combat.isDead hp'

      origin :: Point V2 CInt
      origin = axialToPixelCam cam coord - P (V2 (pixelsPerHealth * fromIntegral maxHealth `div` 2) 20)

      coord :: Axial
      coord = tile ^. tile_coordinate

      health :: Maybe Combat.Health
      health = preview (tile_content . _Just . tc_unit . Combat.unit_hp) tile

      maxW :: CInt
      maxW = pixelsPerHealth * fromIntegral maxHealth

      fillW :: CInt
      fillW = pixelsPerHealth * fromIntegral (fromMaybe 0 health)

      bgRect :: Rectangle CInt
      bgRect = Rectangle origin (V2 maxW barHeight)

      fillRect' :: Rectangle CInt
      fillRect' = Rectangle origin (V2 fillW barHeight)
