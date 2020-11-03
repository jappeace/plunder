{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Button where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import Layer

ffor2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up
  :: Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)

data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving Eq

buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown       = ButtonStateDown
  | otherwise    = ButtonStateOver

button
  :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
  => m (Event t ButtonState)
button = do
  evMotionData <- getMouseMotionEvent
  let position = V2 100 100
      size     = V2 100 100
      V2 tlx tly = position
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
