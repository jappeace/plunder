module Plunder.Render.Arrow (drawArrow) where

import           Control.Monad.IO.Class (MonadIO)
import           Foreign.C.Types        (CInt)
import           Reflex.SDL2            (Renderer, V2 (..), Point (..))
import           SDL.Primitive          (Color, fillTriangle, thickLine)

-- | Draw an arrow (thick shaft + filled arrowhead) from one pixel position
--   to another.  Does nothing when the two points coincide.
drawArrow
  :: MonadIO m
  => Renderer
  -> Point V2 CInt  -- ^ from
  -> Point V2 CInt  -- ^ to
  -> Color
  -> m ()
drawArrow renderer (P (V2 fx fy)) (P (V2 tx ty)) color
  | fx == tx && fy == ty = pure ()
  | otherwise = do
      thickLine renderer (V2 fx fy) (V2 tx ty) 3 color
      fillTriangle renderer tip wing1 wing2 color
  where
    dx, dy, len, ux, uy :: Double
    dx  = fromIntegral (tx - fx)
    dy  = fromIntegral (ty - fy)
    len = sqrt (dx * dx + dy * dy)
    ux  = dx / len
    uy  = dy / len

    arrowSize :: Double
    arrowSize = 15.0

    -- Base of the arrowhead, stepped back from the tip along the shaft.
    bx, by :: Double
    bx = fromIntegral tx - ux * arrowSize
    by = fromIntegral ty - uy * arrowSize

    -- Wing offsets (perpendicular to shaft direction).
    wx, wy :: Double
    wx = (-uy) * (arrowSize * 0.6)
    wy = ux    * (arrowSize * 0.6)

    tip, wing1, wing2 :: V2 CInt
    tip   = V2 tx ty
    wing1 = V2 (round (bx + wx)) (round (by + wy))
    wing2 = V2 (round (bx - wx)) (round (by - wy))
