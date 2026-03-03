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

bannerW, bannerH :: CInt
bannerW = 500
bannerH = 140

msgStyle :: Style
msgStyle = defaultStyle
  & styleColorLens           .~ V4 220 50 50 255
  & styleHorizontalAlignLens .~ Center

-- | Render a full-width banner overlay for game-over / victory states.
--   alphaDyn drives a fade-in (0 = transparent, 255 = fully opaque).
--   winSizeDyn carries the current window dimensions so the banner
--   stays centred even after the user resizes the window.
--   Call this last in the render pipeline so it sits on top of everything.
renderBanner
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t GamePhase -> Dynamic t Word8 -> Dynamic t (V2 CInt) -> m ()
renderBanner font phaseDyn alphaDyn winSizeDyn = do
  renderer <- ask
  -- Pre-allocate text textures once; never recreated per frame.
  diedImg <- allocateText font msgStyle "YOU DIED"
  victImg <- allocateText font msgStyle "YOU ARE VICTORIOUS"
  let combined = (,,) <$> phaseDyn <*> alphaDyn <*> winSizeDyn
  commitLayer $ ffor combined $ \(phase, alpha, V2 w h) -> case phase of
    Playing -> pure ()
    _ -> do
      let surf = case phase of
                   YouDied       -> diedImg
                   YouVictorious -> victImg
          centerPt = P $ V2 (w `div` 2) (h `div` 2)
          img      = surfaceToSettings surf centerPt
          tex      = _image_content img
          bannerRect = Rectangle
            (P $ V2 ((w - bannerW) `div` 2) ((h - bannerH) `div` 2))
            (V2 bannerW bannerH)
      setDrawColor renderer (V4 20 20 20 alpha)
      fillRect renderer (Just bannerRect)
      textureAlphaMod tex $= alpha
      copy renderer tex Nothing (Just $ img ^. image_position)
