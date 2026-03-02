{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Plunder.Guest(guest) where

import           Control.Lens
import           Control.Monad                   (void)
import           Control.Monad.Reader            (MonadReader (..))
import           Control.Monad.Trans.Random.Lazy
import           Plunder.Render.Layer
import           Reflex
import           Reflex.SDL2 hiding (Playing) -- avoid clash with SDL.Audio.Playing
import           Plunder.Render
import           Plunder.State
import           System.Random
import Plunder.Mouse
import Plunder.Shop
import Plunder.Render.Font(defaultFont, bigFont)
import Plunder.Render.Shop
import Plunder.Render.Inventory
import Plunder.Render.Banner
import Data.Maybe (isJust)
import Control.Concurrent (forkIO, threadDelay)

guest
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m => MonadReader Renderer m => m ()
guest = mdo
  -- Print some stuff after the network is built.
  evPB           <- getPostBuild
  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."

  font <- defaultFont
  bannerFont <- bigFont

  gameState <- mkGameState shopEvt
  renderState font gameState
  shopEvt <- renderShop font
    (view game_shop <$> gameState)
    (view (game_player_inventory . inventory_money) <$> gameState)
    (isJust . findFreeAdjacent <$> gameState)
  renderInventory font (view game_inventory_open <$> gameState) (view (game_player_inventory . inventroy_item) <$> gameState)
  renderBanner bannerFont (view game_phase <$> gameState)
  pure ()



makeRandomNT :: forall m a . MonadIO m => m (RandTNT a)
makeRandomNT =
  newStdGen <&> \stdgen -> MkRandTNT (\inner -> fst <$> runRandT inner stdgen)

mkGameState :: forall t m . ReflexSDL2 t m => Event t ShopAction -> m (Dynamic t GameState)
mkGameState shopActions = do

  -- figured these out with getAnySDLEvent and see which needed to redraw
  windowExposedEvt <- getWindowExposedEvent
  windowSizeChangedEvt <- getWindowSizeChangedEvent

  mouseButtonEvt <- getMouseButtonEvent
  keyboardEvt <- getKeyboardEvent

  -- External trigger: fired from IO after the banner timer expires
  (resetEvt, fireReset) <- newTriggerEvent

  let leftClickEvts :: Event t MouseButtonEventData
      leftClickEvts = ffilter (has (mouseButtons . leftClick)) mouseButtonEvt
      rightClickEvts :: Event t MouseButtonEventData
      rightClickEvts = ffilter (\x -> has (mouseButtons . rightClick) x
                               && has (mouseMotion . _Pressed) x
                               ) mouseButtonEvt
      leftClickAxial = calcMouseClickAxial <$> leftClickEvts
      rightClickAxialEvt = calcMouseClickAxial <$> rightClickEvts
      toggleInvEvt :: Event t ()
      toggleInvEvt = () <$ ffilter (\kd ->
          has _Pressed (keyboardEventKeyMotion kd) &&
          not (keyboardEventRepeat kd) &&
          keysymKeycode (keyboardEventKeysym kd) == KeycodeI
        ) keyboardEvt
      events = leftmost [ ShopUpdates <$> shopActions
                        , LeftClick <$> leftClickAxial
                        , RightClick <$> rightClickAxialEvt
                        , Redraw <$ windowSizeChangedEvt
                        , Redraw <$ windowExposedEvt
                        , ToggleInventory <$ toggleInvEvt
                        , ResetGame <$ resetEvt
                        ]
  performEvent_ $ ffor events $ liftIO . print

  ntDyn <- holdView makeRandomNT $ makeRandomNT <$ events
  state <- accumDyn updateState initialState ((,) <$> current ntDyn <@> events)

  -- When the phase *transitions* into a game-over state, start a 5-second
  -- timer then fire ResetGame to restore initial state.
  phaseDyn <- holdUniqDyn (view game_phase <$> state)
  performEvent_ $ ffor (ffilter (/= Playing) (updated phaseDyn)) $ \_ ->
    liftIO $ void $ forkIO $ do
      threadDelay 5000000
      fireReset ResetGame

  performEvent_ $ ffor (describeState <$> updated state) $ liftIO . print

  pure state
