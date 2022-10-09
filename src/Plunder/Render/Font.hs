{-# LANGUAGE TemplateHaskell       #-}

-- | Deal with fonts
module Plunder.Render.Font(defaultFont, module X) where

import Data.FileEmbed
import SDL.Font as X
import Data.ByteString
import Control.Monad.IO.Class

defaultFont :: MonadIO m => m Font
defaultFont = decode robotoFile 20

robotoFile :: ByteString
robotoFile = $(embedFile "assets/roboto.ttf")
