{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell #-}

module Button(button,
              ButtonState(..),
              ButtonSettings,
              defButton, button_postion, button_size) where

import           Control.Monad.Reader (MonadReader (..))
import           Reflex
import           Reflex.SDL2
import Layer
import Control.Lens
import Foreign.C.Types(CInt)

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving Eq

buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown       = ButtonStateDown
  | otherwise    = ButtonStateOver

data ButtonSettings = ButtonSettings
  { _button_postion :: V2 CInt
  , _button_size :: V2 CInt
  }
makeLenses ''ButtonSettings

defButton :: ButtonSettings
defButton = ButtonSettings
  { _button_postion = V2 50 50
  , _button_size = V2 100 50
  }

button
  :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
  => ButtonSettings -> m (Event t ButtonState)
button settings = do
  evMotionData <- getMouseMotionEvent
  let V2 tlx tly = position
      V2 brx bry = position + size
      evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
      evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
        (x >= tlx && x <= brx) && (y >= tly && y <= bry)
  dMouseIsInside <- holdDyn False evMouseIsInside

  evBtn <- getMouseButtonEvent
  let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
  dButtonIsDown <- holdDyn False evBtnIsDown

  let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
  evPB         <- getPostBuild
  dButtonState <- holdDyn ButtonStateUp $ leftmost [ updated dButtonStatePre
                                                   , ButtonStateUp <$ evPB
                                                   ]
  r <- ask
  commitLayer $ ffor dButtonState $ \st -> do
    let color = case st of
                  ButtonStateUp   -> V4 192 192 192 255
                  ButtonStateOver -> 255
                  ButtonStateDown -> V4 128 128 128 255
    rendererDrawColor r $= color
    fillRect r $ Just $ Rectangle (P position) size

  updated <$> holdUniqDyn dButtonState
  where
      position = settings ^. button_postion
      size     = settings ^. button_size
