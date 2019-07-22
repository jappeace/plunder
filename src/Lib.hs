{-# LANGUAGE OverloadedStrings #-}

module Lib(libF) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Foreign.C.Types
import qualified SDL
import           SDL.Vect



screenWidth :: CInt
screenHeight = 480
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

  helloWorld <- SDL.loadBMP "img/start.bmp"

  SDL.surfaceBlit helloWorld Nothing screenSurface Nothing
  SDL.updateWindowSurface window

  threadDelay 2000000
  forever $ print "hello -world"

  SDL.destroyWindow window
  SDL.quit

screenHeight :: CInt
