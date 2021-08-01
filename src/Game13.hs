{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Contains all reflex-sdl boilerplate
module Game13(libF) where

import           Control.Monad        (void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import Guest
import qualified SDL.Font as Font

app :: (ReflexSDL2 t m, MonadReader Renderer m) =>  m ()
app = do
  (_, dynLayers) <- runDynamicWriterT $ do
    guest
    onQuit
  r <- ask
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    rendererDrawColor r $= V4 0 0 0 255
    clear r
    sequence_ layers
    present r

libF :: IO ()
libF = do
  initializeAll
  Font.initialize
  let ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
      cfg = defaultWindow{ windowGraphicsContext = OpenGLContext ogl
                         , windowResizable       = True
                         , windowHighDPI         = False
                         , windowInitialSize     = V2 640 480
                         }
  window <- createWindow "game-13" cfg
  void $ glCreateContext window

  putStrLn "creating renderer!"
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  -- Host the network with an example of how to embed your own effects.
  -- In this case it's a simple reader.
  host $ (runReaderT app r)
  destroyRenderer r
  destroyWindow window
  quit
  Font.quit

onQuit :: ReflexSDL2 t m => m ()
onQuit = do
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit
