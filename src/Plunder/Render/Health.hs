
module Plunder.Render.Health(healthBar) where

import qualified Plunder.Combat as Combat
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Data.Maybe
import           Plunder.Grid
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Layer

healthBar :: DynamicWriter t [Layer m] m
        => ReflexSDL2 t m
        => MonadReader Renderer m
        => Dynamic t Tile -> m ()
healthBar tileDyn = do
  renderer    <- ask
  commitLayer $ healthBar' renderer <$> tileDyn

healthBar' :: MonadIO m
        => Renderer -> Tile -> m ()
healthBar' renderer tile = unless isDead $ do
    drawRect renderer $ Just rectangle
    where
      isDead :: Bool
      isDead = fromMaybe True $ do
        hp' <- tile ^? tile_content . _Just . tc_unit . Combat.unit_hp
        pure $ Combat.isDead hp'

      rectangle = Rectangle (axialToPixel coord - P (V2 60 20)) (V2 healthPixelSize 2)

      pixelsPerHealth = 12

      coord :: Axial
      coord = tile ^. tile_coordinate

      -- pixels
      healthPixelSize = fromIntegral $ pixelsPerHealth * fromMaybe 0 health

      health :: Maybe Combat.Health
      health = preview (tile_content . _Just . tc_unit . Combat.unit_hp) tile
