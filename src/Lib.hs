{-# LANGUAGE OverloadedStrings #-}

module Lib(libF) where

import           Control.Concurrent (threadDelay)
import           Foreign.C.Types
import qualified SDL
import           SDL.Vect

screenWidth :: CInt
screenHeight = 480
screenHeight :: CInt
screenWidth = 640

libF :: IO ()
libF = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.quit
