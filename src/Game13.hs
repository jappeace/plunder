{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Game13 where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import Guest
import Layer

app :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
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
  host $ runReaderT app r
  destroyRenderer r
  destroyWindow window
  quit

onQuit :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
  => m ()
onQuit = do
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit