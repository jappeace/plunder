{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Banner(renderBanner) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.Text            (Text)
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
bannerH = 110

-- | Render a full-width banner overlay for game-over / victory states.
--   winSizeDyn carries the current window dimensions so the banner
--   stays centred even after the user resizes the window.
--   Call this last in the render pipeline so it sits on top of everything.
renderBanner
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t GamePhase -> Dynamic t (V2 CInt) -> m ()
renderBanner font phaseDyn winSizeDyn = do
  renderer <- ask
  let combined = (,) <$> phaseDyn <*> winSizeDyn
  commitLayer $ renderBackground renderer <$> combined
  void $ image =<< holdDyn Nothing =<< dynView (renderMessage font winSizeDyn <$> phaseDyn)

renderBackground :: MonadIO m => Renderer -> (GamePhase, V2 CInt) -> m ()
renderBackground renderer (phase, V2 w h) = case phase of
  Playing -> pure ()
  _ -> do
    setDrawColor renderer $ V4 20 20 20 255
    fillRect renderer (Just (Rectangle (P $ V2 ((w - bannerW) `div` 2) ((h - bannerH) `div` 2))
                                       (V2 bannerW bannerH)))

renderMessage
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> Dynamic t (V2 CInt) -> GamePhase -> m (Maybe ImageSettings)
renderMessage _ _ Playing = pure Nothing
renderMessage font winSizeDyn phase = do
  V2 w h <- sample (current winSizeDyn)
  Just <$> renderText font style (P $ V2 (w `div` 2) (h `div` 2)) msg
  where
    msg :: Text
    msg = case phase of
      YouDied       -> "YOU DIED"
      YouVictorious -> "YOU ARE VICTORIOUS"
    style = defaultStyle
      & styleColorLens           .~ V4 220 50 50 255
      & styleHorizontalAlignLens .~ Center
