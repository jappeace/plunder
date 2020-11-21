{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Guest where

import           Control.Monad.Reader (MonadReader (..))
import           Reflex
import           Reflex.SDL2
import Layer
import Hexagon
import Data.Foldable
import Grid

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

  traverse_ (hexagon . (renderTile defaultSize)) $ unGrid initialGrid
