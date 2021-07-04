{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Render(renderState) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader  (MonadReader (..))
import           Data.Bool
import           Grid
import           Hexagon
import           Image
import           Layer
import           Reflex
import           Reflex.SDL2
import           State

renderState :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Dynamic t GameState -> m ()
renderState state = do
  vikingF <- renderImage <$> loadViking
  enemyF <- renderImage <$> loadEnemy
  void $ listWithKey (view game_board <$> state) $ \axial _ -> do
    hexagon $ renderHex axial

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> mapMaybe (view game_selected) (updated state)

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn -> do
    let playerSettings = bool Nothing (Just axial)
                       . has (tile_content . _Just . _Player) <$> tileDyn
    let enemySettings = bool Nothing (Just axial)
                       . has (tile_content . _Just . _Enemy) <$> tileDyn
    performEvent_ $ ffor (updated playerSettings) (maybe (pure ()) $ liftIO . print)

    image $ fmap vikingF <$> playerSettings
    image $ fmap enemyF <$> enemySettings


renderSelected :: Axial -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex
