{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Terrain
  ( renderTerrain
  , renderFogOverlay
  ) where

import qualified Data.Foldable        as Foldable
import qualified Data.Map.Strict      as Map
import qualified Data.Vector.Storable as S
import           Control.Lens
import           Data.Int             (Int16)
import           Foreign.C.Types      (CInt)
import           Plunder.Grid
import           Plunder.State (GameState, Visibility(Visible, Fog, Unexplored), tileVisibility, game_camera)
import           Plunder.Render.Layer
import           Plunder.Render.RenderFun (RenderFun(..))
import           Reflex
import           Reflex.SDL2
import           SDL.Primitive        (Color)

-- | RGBA colour for each terrain type.
terrainToColor :: Terrain -> Color
terrainToColor Land      = V4  86 168  86 255  -- grass green
terrainToColor Water     = V4  65 105 225 255  -- royal blue
terrainToColor Mountains = V4 148 128 115 255  -- stone grey

-- | Axial range rendered as terrain (covers the visible screen and one-hex
--   border on each side so that "outside the grid" shows as water).
terrainCoords :: [Axial]
terrainCoords = [MkAxial q r | q <- [-1 .. 7], r <- [-1 .. 7]]

-- | Compute the six polygon corners of a pointy-top hexagon at the given
--   axial coordinate, with a camera pixel offset applied.
hexPolyPoints :: V2 CInt -> Axial -> (S.Vector Int16, S.Vector Int16)
hexPolyPoints cam axial = (S.fromList xs, S.fromList ys)
  where
    P (V2 cx cy) = axialToPixelCam cam axial
    -- Pointy-top hexagon corners at 330°, 30°, 90°, 150°, 210°, 270°.
    cornerDegrees :: [Double]
    cornerDegrees = [330, 30, 90, 150, 210, 270]
    toCoord deg =
      let rad = pi / 180.0 * deg
          px  = cx + (floor (fromIntegral hexSize * cos rad) :: CInt)
          py  = cy + (floor (fromIntegral hexSize * sin rad) :: CInt)
      in (fromIntegral px :: Int16, fromIntegral py :: Int16)
    corners = map toCoord cornerDegrees
    xs = map fst corners
    ys = map snd corners

-- | Render terrain-filled hexagons for all coordinates in 'terrainCoords'.
--   Coordinates absent from the grid are drawn with the Water colour so that
--   the area beyond the map boundary looks like ocean.
renderTerrain :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => RenderFun
  -> Dynamic t (V2 CInt)
  -> Dynamic t Grid
  -> m ()
renderTerrain rf cameraDyn boardDyn =
  commitLayer $ ffor2 cameraDyn boardDyn $ \cam board ->
    Foldable.for_ terrainCoords $ \axial ->
      let color = case Map.lookup axial board of
                    Nothing   -> terrainToColor Water
                    Just tile -> terrainToColor (tile ^. tile_terrain)
          (xs, ys) = hexPolyPoints cam axial
      in rf_fillPolygon rf xs ys color

-- | Render a fog-of-war overlay on top of terrain and sprites.
--   Tiles close to a player are fully visible, farther tiles get a
--   semi-transparent or fully opaque dark overlay.
renderFogOverlay :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => RenderFun -> Dynamic t GameState -> m ()
renderFogOverlay rf stateDyn =
  commitLayer $ ffor stateDyn $ \gs ->
    let cam = gs ^. game_camera
    in Foldable.for_ terrainCoords $ \axial ->
      let (xs, ys) = hexPolyPoints cam axial
      in case tileVisibility gs axial of
        Visible -> pure ()
        Fog     -> rf_fillPolygon rf xs ys (V4 0 0 0 128)
        Unexplored -> rf_fillPolygon rf xs ys (V4 0 0 0 255)
