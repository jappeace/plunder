{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Contains all reflex-sdl boilerplate
module Plunder.Game13(main) where


import Foreign.C.String
import SDL.Raw.Error
import           Control.Monad        (void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import           Plunder.Guest
import           qualified SDL.Font as Font

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

main :: IO ()
main = do
  initializeAll
  putStrLn "initializing"
  Font.initialize
  let ogl = defaultOpenGL{ glProfile = Compatibility Normal 4 6 }
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
