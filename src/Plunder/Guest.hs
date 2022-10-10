{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Plunder.Guest(guest) where

import           Control.Lens
import           Control.Monad.Reader            (MonadReader (..))
import           Control.Monad.Trans.Random.Lazy
import           Plunder.Render.Layer
import           Reflex
import           Reflex.SDL2
import           Plunder.Render
import           Plunder.State
import           System.Random
import Plunder.Mouse

guest
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m => MonadReader Renderer m => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB           <- getPostBuild
  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."
  gameState <- mkGameState
  renderState gameState


makeRandomNT :: forall m a . MonadIO m => m (RandTNT a)
makeRandomNT =
  newStdGen <&> \stdgen -> MkRandTNT (\inner -> fst <$> runRandT inner stdgen)

mkGameState :: forall t m . ReflexSDL2 t m => m (Dynamic t GameState)
mkGameState = do

  -- figured these out with getAnySDLEvent and see which needed to redraw
  windowExposedEvt <- getWindowExposedEvent
  windowSizeChangedEvt <- getWindowSizeChangedEvent

  mouseButtonEvt <- getMouseButtonEvent
  let leftClickEvts :: Event t MouseButtonEventData
      leftClickEvts = ffilter (has (mouseButtons . leftClick)) mouseButtonEvt
      rightClickEvts :: Event t MouseButtonEventData
      rightClickEvts = ffilter (\x -> has (mouseButtons . rightClick) x
                               && has (mouseMotion . _Pressed) x
                               ) mouseButtonEvt
      leftClickAxial = calcMouseClickAxial <$> leftClickEvts
      rightClickAxialEvt = calcMouseClickAxial <$> rightClickEvts
      events = leftmost [ LeftClick <$> leftClickAxial
                        , RightClick <$> rightClickAxialEvt
                        , Redraw <$ windowSizeChangedEvt
                        , Redraw <$ windowExposedEvt
                        ]
  performEvent_ $ ffor events $ liftIO . print

  ntDyn <- holdView makeRandomNT $ makeRandomNT <$ events
  state <- accumDyn updateState initialState ((,) <$> current ntDyn <@> events)


  performEvent_ $ ffor (describeState <$> updated state) $ liftIO . print

  pure state
