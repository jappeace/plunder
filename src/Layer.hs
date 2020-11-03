{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Layer where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2


type Layer m = Performable m ()

commitLayers :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m)
      => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn

commitLayer :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure
