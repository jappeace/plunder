{-# LANGUAGE DataKinds #-}

module Plunder.Render.Help(renderHelp) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.Text            (Text)
import           Foreign.C.Types      (CInt)
import           Plunder.Render.Color
import           Plunder.Render.Font
import           Plunder.Render.Image (ImageSettings (..), image, image_position)
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Reflex
import           Reflex.SDL2

data LineStyle = Header | Body | Dim

helpLines :: [(Text, LineStyle)]
helpLines =
  [ ("Controls",                          Header)
  , (" ",                                 Body)
  , ("Left click   select unit",          Body)
  , ("Right click  plan move / attack",   Body)
  , ("Right click  open nearby shop",     Body)
  , ("Space        end turn",             Body)
  , ("I            toggle inventory",     Body)
  , ("Click item   use from inventory",   Body)
  , (" ",                                 Body)
  , ("Press Enter or click OK to start",  Dim)
  ]

toStyle :: LineStyle -> Style
toStyle Header = defaultStyle & styleColorLens .~ V4 0   0   0   255
toStyle Body   = defaultStyle & styleColorLens .~ V4 40  40  40  255
toStyle Dim    = defaultStyle & styleColorLens .~ V4 120 120 120 255

bgW :: CInt
bgW = 330

bgH :: CInt
bgH = 260

lineH :: CInt
lineH = 20

textPadX :: CInt
textPadX = 14

textPadY :: CInt
textPadY = 14

-- | Render the help overlay.  Returns an event that fires when the OK
--   button inside the overlay is clicked.
renderHelp
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t Bool -> Dynamic t (V2 CInt) -> m (Event t ())
renderHelp font isOpen winSizeDyn = do
  renderer <- ask
  surfaces <- forM helpLines $ \(text, ls) -> allocateText font (toStyle ls) text
  okSurface <- allocateText font (toStyle Body) "[ OK ]"
  let V2 okW okH = textSurfaceSize okSurface
      combined   = (,) <$> isOpen <*> winSizeDyn
      okImgDyn   = ffor combined $ \(open, V2 w h) ->
        if open
        then let bgX = (w - bgW) `div` 2
                 bgY = (h - bgH) `div` 2
                 okPos = P $ V2 (bgX + (bgW - okW) `div` 2) (bgY + bgH - okH - 10)
             in Just (surfaceToSettings okSurface okPos)
        else Nothing
  commitLayer $ ffor combined $ \(open, V2 w h) -> when open $ do
    let bgX = (w - bgW) `div` 2
        bgY = (h - bgH) `div` 2
        textX = bgX + textPadX
        textY idx = bgY + textPadY + fromIntegral idx * lineH
    setDrawColor renderer (V4 210 210 210 245)
    fillRect renderer $ Just (Rectangle (P $ V2 bgX bgY) (V2 bgW bgH))
    setDrawColor renderer (V4 0 0 0 255)
    drawRect renderer $ Just (Rectangle (P $ V2 bgX bgY) (V2 bgW bgH))
    forM_ (zip [(0 :: Int) ..] surfaces) $ \(idx, surf) ->
      let img = surfaceToSettings surf (P $ V2 textX (textY idx))
      in  copy renderer (_image_content img) Nothing (Just $ img ^. image_position)
  okClicks <- image okImgDyn
  pure (() <$ okClicks)
