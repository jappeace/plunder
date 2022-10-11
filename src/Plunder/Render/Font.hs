{-# LANGUAGE TemplateHaskell       #-}

-- | Deal with fonts
module Plunder.Render.Font(defaultFont, module X) where

import Data.FileEmbed
import SDL.Font as X hiding (Style)
import Data.ByteString
import Control.Monad.IO.Class

defaultFont :: MonadIO m => m Font
defaultFont = decode notoSans 20

_robotoFile :: ByteString
_robotoFile = $(embedFile "assets/roboto.ttf")

notoSans :: ByteString
notoSans = $(embedFile "assets/notosans-black.ttf")
