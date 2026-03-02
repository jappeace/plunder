{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Banner(renderBanner) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.Text            (Text)
import           Plunder.Render.Color
import           Plunder.Render.Font
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Plunder.State        (GamePhase (..))
import           Reflex
import           Reflex.SDL2 hiding (Playing) -- avoid clash with SDL.Audio.Playing

-- | Render a full-width banner overlay for game-over / victory states.
--   Call this last in the render pipeline so it sits on top of everything.
renderBanner
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t GamePhase -> m ()
renderBanner font phaseDyn = do
  renderer <- ask
  commitLayer $ renderBackground renderer <$> phaseDyn
  void $ image =<< holdDyn Nothing =<< dynView (renderMessage font <$> phaseDyn)

renderBackground :: MonadIO m => Renderer -> GamePhase -> m ()
renderBackground renderer phase = case phase of
  Playing -> pure ()
  _ -> do
    setDrawColor renderer $ V4 20 20 20 255
    fillRect renderer (Just (Rectangle (P $ V2 50 190) (V2 500 110)))

renderMessage
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> GamePhase -> m (Maybe ImageSettings)
renderMessage _ Playing = pure Nothing
renderMessage font phase = Just <$> renderText font style (P $ V2 300 230) msg
  where
    msg :: Text
    msg = case phase of
      YouDied       -> "YOU DIED"
      YouVictorious -> "YOU ARE VICTORIOUS"
    style = defaultStyle
      & styleColorLens           .~ V4 220 50 50 255
      & styleHorizontalAlignLens .~ Center
