
module Render.Health(healthBar) where

import           Combat
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Data.Maybe
import           Grid
import           Reflex
import           Reflex.SDL2
import           Render.Layer

healthBar :: DynamicWriter t [Layer m] m
        => ReflexSDL2 t m
        => MonadReader Renderer m
        => Dynamic t Tile -> m ()
healthBar tileDyn = do
  renderer    <- ask
  commitLayer $ healthBar' renderer <$> tileDyn

healthBar' :: MonadIO m
        => Renderer -> Tile -> m ()
healthBar' renderer tile = do
    drawRect renderer $ Just rectangle
    where
      rectangle = Rectangle (axialToPixel coord) (V2 healthSize 2)

      maxHealth = 10

      coord :: Axial
      coord = tile ^. tile_coordinate

      -- pixels
      healthSize = fromIntegral $ (((maxHealth * 10) * fromMaybe 0 health) `quot` 10) * 2

      health :: Maybe Health
      health = preview (tile_content . _Just . tc_unit . unit_hp) tile
