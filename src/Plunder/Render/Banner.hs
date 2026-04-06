{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Banner(renderBanner) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader (..))
import           Plunder.Render.RenderFun (RenderFun(..))
import           Foreign.C.Types      (CInt)
import           Plunder.Render.Font
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Plunder.RenderState  (BannerState(..))
import           Reflex
import           Reflex.SDL2

bannerW, bannerH :: CInt
bannerW = 500
bannerH = 140

msgStyle :: Style
msgStyle = defaultStyle
  & styleColorLens           .~ V4 220 50 50 255
  & styleHorizontalAlignLens .~ Center

-- | Render a full-width banner overlay for game-over / victory states.
--   'BannerState' carries the phase and fade alpha together.
--   winSizeDyn carries the current window dimensions so the banner
--   stays centred even after the user resizes the window.
--   Call this last in the render pipeline so it sits on top of everything.
renderBanner
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader RenderFun m
  => Font -> Dynamic t BannerState -> Dynamic t (V2 CInt) -> m ()
renderBanner font bannerDyn winSizeDyn = do
  MkRenderFun{rf_setDrawColor, rf_fillRect, rf_copy} <- ask
  -- Pre-allocate text textures once; never recreated per frame.
  diedImg <- allocateText font msgStyle "YOU DIED"
  victImg <- allocateText font msgStyle "YOU ARE VICTORIOUS"
  let combined = (,) <$> bannerDyn <*> winSizeDyn
  commitLayer $ ffor combined $ \(banner, V2 w h) -> case banner of
    NoBanner -> pure ()
    DeathBanner alpha -> renderOverlay rf_setDrawColor rf_fillRect rf_copy diedImg alpha w h
    VictoryBanner alpha -> renderOverlay rf_setDrawColor rf_fillRect rf_copy victImg alpha w h
  where
    renderOverlay setColor fillR copyTex surf alpha w h = do
      let centerPt = P $ V2 (w `div` 2) (h `div` 2)
          img      = surfaceToSettings surf centerPt
          tex      = _image_content img
          bannerRect = Rectangle
            (P $ V2 ((w - bannerW) `div` 2) ((h - bannerH) `div` 2))
            (V2 bannerW bannerH)
      _ <- setColor (V4 20 20 20 alpha)
      _ <- fillR (Just bannerRect)
      textureAlphaMod tex $= alpha
      copyTex tex Nothing (Just $ img ^. image_position)
