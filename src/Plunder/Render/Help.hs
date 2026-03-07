{-# LANGUAGE DataKinds #-}

module Plunder.Render.Help(renderHelp) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.List            (foldl')
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

data HelpLine
  = HelpHeader Text
  | HelpRow Text Text          -- ^ key, description
  | HelpSpacer
  | HelpFooter Text

helpLines :: [HelpLine]
helpLines =
  [ HelpHeader "Controls"
  , HelpSpacer
  , HelpRow "Left click"  "select unit"
  , HelpRow "Right click" "plan move / attack"
  , HelpRow "Right click" "open nearby shop"
  , HelpRow "Space"       "end turn"
  , HelpRow "I"           "toggle inventory"
  , HelpRow "Click item"  "use from inventory"
  , HelpSpacer
  , HelpFooter "Press Enter or click OK to start"
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

-- | A rendered help line, ready to blit.
data RenderedLine
  = RenderedSingle TextSurface LineStyle
  | RenderedPair   TextSurface TextSurface  -- ^ key surface, description surface
  | RenderedBlank

allocateLine
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> HelpLine -> m RenderedLine
allocateLine font (HelpHeader t) = RenderedSingle <$> allocateText font (toStyle Header) t <*> pure Header
allocateLine font (HelpRow k d)  = RenderedPair <$> allocateText font (toStyle Body) k
                                                <*> allocateText font (toStyle Body) d
allocateLine _    HelpSpacer     = pure RenderedBlank
allocateLine font (HelpFooter t) = RenderedSingle <$> allocateText font (toStyle Dim) t <*> pure Dim

-- | Column gap between key and description columns.
colGap :: CInt
colGap = 16

-- | Compute the width of the key column (widest key surface).
keyColumnWidth :: [RenderedLine] -> CInt
keyColumnWidth = foldl' maxKey 0
  where
    maxKey acc (RenderedPair keySurf _) = max acc (let V2 w _ = textSurfaceSize keySurf in w)
    maxKey acc (RenderedSingle _ _)     = acc
    maxKey acc RenderedBlank            = acc

-- | Render the help overlay.  Returns an event that fires when the OK
--   button inside the overlay is clicked.
renderHelp
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t Bool -> Dynamic t (V2 CInt) -> m (Event t ())
renderHelp font isOpen winSizeDyn = do
  renderer <- ask
  rendered <- forM helpLines $ allocateLine font
  okSurface <- allocateText font (toStyle Body) "[ OK ]"
  let V2 okW okH = textSurfaceSize okSurface
      keyColW    = keyColumnWidth rendered
      descX      = textPadX + keyColW + colGap
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
    forM_ (zip [(0 :: Int) ..] rendered) $ \(idx, rl) -> case rl of
      RenderedSingle surf _ -> do
        let img = surfaceToSettings surf (P $ V2 textX (textY idx))
        copy renderer (_image_content img) Nothing (Just $ img ^. image_position)
      RenderedPair keySurf descSurf -> do
        let kImg = surfaceToSettings keySurf (P $ V2 textX (textY idx))
            dImg = surfaceToSettings descSurf (P $ V2 (bgX + descX) (textY idx))
        copy renderer (_image_content kImg) Nothing (Just $ kImg ^. image_position)
        copy renderer (_image_content dImg) Nothing (Just $ dImg ^. image_position)
      RenderedBlank -> pure ()
  okClicks <- image okImgDyn
  pure (() <$ okClicks)
