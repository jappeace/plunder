{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( libF
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Foldable          (traverse_)
import           Foreign.C.Types
import qualified SDL
import           SDL.Vect

data Assets = Assets
  { _assets_surfaces :: [SDL.Surface]
  , _assets_screen   :: SDL.Surface
  , _assets_window   :: SDL.Window
  }

makeLenses ''Assets

data Movement
  = Left
  | Right
  | SpeedUp
  | SpeedDown

gameLoop :: Assets -> IO ()
gameLoop assets = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  traverse_ (\xOut -> SDL.surfaceBlit xOut Nothing (assets ^. assets_screen) Nothing) $
    assets ^. assets_surfaces
  SDL.updateWindowSurface $ assets ^. assets_window
  unless quit $ gameLoop assets

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

libF :: IO ()
libF = do
  SDL.initialize [SDL.InitVideo]
  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window
  bg <- SDL.loadBMP "img/start.bmp"
  mainChar <- SDL.loadBMP "img/main-char.bmp"
  gameLoop $
    Assets
      { _assets_surfaces = [bg, mainChar]
      , _assets_screen = screenSurface
      , _assets_window = window
      }
  SDL.destroyWindow window
  SDL.quit
