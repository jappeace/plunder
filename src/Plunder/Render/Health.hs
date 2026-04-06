
module Plunder.Render.Health(healthBar) where

import qualified Plunder.Combat as Combat
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (MonadReader (..))
import           Plunder.Render.RenderFun (RenderFun(..))
import           Foreign.C.Types        (CInt)
import qualified Unwitch.Convert.Int as Int
import           Plunder.Grid (Axial, axialToPixelCam)
import           Plunder.RenderState (RenderTile, rtile_coordinate, rtile_healthBar, hb_currentHp, hb_maxHp)
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Layer
import SDL.Primitive(Color)

healthBar :: DynamicWriter t [Layer m] m
        => ReflexSDL2 t m
        => MonadReader RenderFun m
        => Dynamic t (V2 CInt) -> Dynamic t RenderTile -> m ()
healthBar cameraDyn tileDyn = do
  rf <- ask
  commitLayer $ healthBar' rf <$> cameraDyn <*> tileDyn

barHeight :: Num a => a
barHeight = 6

pixelsPerHealth :: Num a => a
pixelsPerHealth = 12

bgColor :: Color
bgColor = V4 40 40 40 220

fillColor :: Color
fillColor = V4 80 200 80 255

borderColor :: Color
borderColor = V4 0 0 0 255

healthBar' :: MonadIO m
        => RenderFun -> V2 CInt -> RenderTile -> m ()
healthBar' rf cam tile = case tile ^. rtile_healthBar of
    Nothing -> pure ()
    Just hbInfo -> do
      let currentHp = hbInfo ^. hb_currentHp
          maxHp     = hbInfo ^. hb_maxHp
      unless (Combat.isDead currentHp) $ do
        -- dark background (full max-health width)
        rf_setDrawColor rf bgColor
        rf_fillRect rf $ Just (bgRect maxHp)
        -- green health fill
        rf_setDrawColor rf fillColor
        rf_fillRect rf $ Just (fillRect' currentHp maxHp)
        -- black border on top
        rf_setDrawColor rf borderColor
        rf_drawRect rf $ Just (bgRect maxHp)
    where
      coord :: Axial
      coord = tile ^. rtile_coordinate

      -- | Health values (0–10) always fit in CInt; 0 default is unreachable.
      toCInt' :: Int -> CInt
      toCInt' = maybe 0 id . Int.toCInt

      barOrigin :: Combat.Health -> Point V2 CInt
      barOrigin maxHp = axialToPixelCam cam coord - P (V2 (pixelsPerHealth * toCInt' maxHp `div` 2) 20)

      bgRect :: Combat.Health -> Rectangle CInt
      bgRect maxHp = Rectangle (barOrigin maxHp) (V2 (pixelsPerHealth * toCInt' maxHp) barHeight)

      fillRect' :: Combat.Health -> Combat.Health -> Rectangle CInt
      fillRect' currentHp maxHp = Rectangle (barOrigin maxHp) (V2 (pixelsPerHealth * toCInt' currentHp) barHeight)
