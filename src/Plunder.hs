{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Contains all reflex-sdl boilerplate
module Plunder(main, app) where

import Foreign.C.String
import SDL.Raw.Error
import           Control.Monad        (void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import           Plunder.Guest
import           Control.Lens ((.~), (&))
import           Plunder.Level (decodeLevelFile, Level)
import           Plunder.Render.RenderFun (RenderFun(..), mkRenderFun)
import           Plunder.State (GameState, levelToGameState, game_levels, game_level_index)
import           qualified SDL.Font as Font

app :: (ReflexSDL2 t m, MonadReader RenderFun m) => GameState -> m (Dynamic t GameState)
app initGS = do
  (gameState, dynLayers) <- runDynamicWriterT $ do
    gs <- guest initGS
    onQuit
    pure gs
  MkRenderFun{rf_clear, rf_present, rf_setRendererDrawColor} <- ask
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    rf_setRendererDrawColor (V4 0 0 0 255)
    rf_clear
    sequence_ layers
    rf_present
  pure gameState

main :: IO ()
main = do
  initializeAll
  putStrLn "initializing"
  Font.initialize

  putStrLn "loading levels..."
  level1Result <- decodeLevelFile "assets/levels/level1.toml"
  level2Result <- decodeLevelFile "assets/levels/level2.toml"
  let loadLevel :: Show e => Either e Level -> Level
      loadLevel (Right lvl) = lvl
      loadLevel (Left err)  = error $ "Failed to load level: " <> show err
      levels = [loadLevel level1Result, loadLevel level2Result]
      initGS = levelToGameState (head levels)
             & game_levels .~ levels
             & game_level_index .~ 0

  let ogl = defaultOpenGL{ glProfile = Compatibility Debug 4 6 }
      cfg = defaultWindow{ windowGraphicsContext = OpenGLContext ogl
                         , windowResizable       = True
                         , windowHighDPI         = False
                         , windowInitialSize     = V2 640 480
                         }
  putStrLn "creating window"
  window <- createWindow "game-13" cfg
  mycError <- getError
  putStrLn =<< peekCString mycError
  print (("context" :: String), window)
  void $ glCreateContext window

  putStrLn "creating renderer!"
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  -- Host the network with an example of how to embed your own effects.
  -- In this case it's a simple reader.
  host $ void $ runReaderT (app initGS) (mkRenderFun r)
  destroyRenderer r
  destroyWindow window
  quit
  Font.quit

onQuit :: ReflexSDL2 t m => m ()
onQuit = do
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit
