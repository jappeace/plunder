{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Guest where

import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Reflex
import           Reflex.SDL2
import Button
import Layer
import Control.Lens
import Hexagon

motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128

renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

guest
  :: ReflexSDL2 t m => DynamicWriter t [Layer m] m => MonadReader Renderer m
  =>  m ()
guest = do
  -- Print some stuff after the network is built.
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  ------------------------------------------------------------------------------
  -- Get a handle on our renderer
  ------------------------------------------------------------------------------
  hexagon defHex

  ------------------------------------------------------------------------------
  -- A button!
  ------------------------------------------------------------------------------
  evBtnState <- button defButton
  let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
  performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button pressed!"

  evButtonTwo <- button $ button_postion .~ V2 300 200 $ defButton
  performEvent_ $ ffor (fmapMaybe (guard . (== ButtonStateDown)) evButtonTwo ) $ const $ liftIO $ putStrLn "Button pressed!"
