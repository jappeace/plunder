{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Banner(renderBanner) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import           Data.Word            (Word8)
import           Foreign.C.Types      (CInt)
import           Plunder.Render.Color
import           Plunder.Render.Font
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Plunder.State        (GamePhase (..))
import           Reflex
import           Reflex.SDL2 hiding (Playing) -- avoid clash with SDL.Audio.Playing

screenW, screenH :: CInt
screenW = 640
screenH = 480

bannerW, bannerH :: CInt
bannerW = 500
bannerH = 110

bannerRect :: Rectangle CInt
bannerRect =
  Rectangle (P $ V2 ((screenW - bannerW) `div` 2) ((screenH - bannerH) `div` 2))
            (V2 bannerW bannerH)

-- Horizontal centre of screen; surfaceToSettings handles the x offset.
textPos :: Point V2 CInt
textPos = P $ V2 (screenW `div` 2) (screenH `div` 2)

msgStyle :: Style
msgStyle = defaultStyle
  & styleColorLens           .~ V4 220 50 50 255
  & styleHorizontalAlignLens .~ Center

-- | Render a full-width banner overlay for game-over / victory states.
--   alphaDyn drives a fade-in (0 = transparent, 255 = fully opaque).
--   Call this last in the render pipeline so it sits on top of everything.
renderBanner
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t GamePhase -> Dynamic t Word8 -> m ()
renderBanner font phaseDyn alphaDyn = do
  renderer <- ask
  -- Pre-allocate text textures once; never recreated per frame.
  diedImg <- surfaceToSettings <$> allocateText font msgStyle "YOU DIED"          <*> pure textPos
  victImg <- surfaceToSettings <$> allocateText font msgStyle "YOU ARE VICTORIOUS" <*> pure textPos
  let combined = (,) <$> phaseDyn <*> alphaDyn
  commitLayer $ ffor combined $ \(phase, alpha) -> case phase of
    Playing -> pure ()
    _ -> do
      let img = case phase of
            YouDied       -> diedImg
            _             -> victImg
          tex = _image_content img
      setDrawColor renderer (V4 20 20 20 alpha)
      fillRect renderer (Just bannerRect)
      textureAlphaMod tex $= alpha
      copy renderer tex Nothing (Just $ img ^. image_position)
