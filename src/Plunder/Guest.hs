{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Plunder.Guest(guest) where

import           Control.Lens
import           Control.Monad                   (forM_, void)
import           Control.Monad.Reader            (MonadReader (..))
import           Data.Word                       (Word8)
import           Foreign.C.Types                 (CInt)
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
import Plunder.Render.Help
import Plunder.Render.Text (defaultStyle, surfaceToSettings, allocateText, textSurfaceSize)
import Plunder.Render.Image (image)
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

  anyMouseEvt <- getMouseButtonEvent
  anyKeyEvt   <- getKeyboardEvent
  helpOpenDyn <- holdDyn True $ False <$ leftmost [() <$ anyMouseEvt, () <$ anyKeyEvt]
  renderHelp font helpOpenDyn

  (gameState, alphaDyn, winSizeDyn, fireEndTurn) <- mkGameState shopEvt inventoryClickEvt
  renderState font gameState
  shopEvt <- renderShop font
    (view game_shop <$> gameState)
    (view (game_player_inventory . inventory_money) <$> gameState)
    (isJust . findFreeAdjacent <$> gameState)
  inventoryClickEvt <- renderInventory font (view game_inventory_open <$> gameState) (view (game_player_inventory . inventroy_item) <$> gameState)
  renderBanner bannerFont (view game_phase <$> gameState) alphaDyn winSizeDyn
  -- End Turn button: bottom-right corner, position tracks window size
  endTurnSurface <- allocateText font defaultStyle "[ End Turn ]"
  let V2 btnW btnH = textSurfaceSize endTurnSurface
      endTurnImgDyn = ffor winSizeDyn $ \(V2 w h) ->
        Just $ surfaceToSettings endTurnSurface (P $ V2 (w - btnW - 10) (h - btnH - 10))
  endTurnClicks <- image endTurnImgDyn
  performEvent_ $ ffor endTurnClicks $ \_ -> liftIO (fireEndTurn ())
  pure ()



makeRandomNT :: forall m a . MonadIO m => m (RandTNT a)
makeRandomNT =
  newStdGen <&> \stdgen -> MkRandTNT (\inner -> fst <$> runRandT inner stdgen)

mkGameState :: forall t m . ReflexSDL2 t m => Event t ShopAction -> Event t ShopItem -> m (Dynamic t GameState, Dynamic t Word8, Dynamic t (V2 CInt), () -> IO ())
mkGameState shopActions inventoryActions = do

  -- figured these out with getAnySDLEvent and see which needed to redraw
  windowExposedEvt <- getWindowExposedEvent
  windowSizeChangedEvt <- getWindowSizeChangedEvent

  mouseButtonEvt <- getMouseButtonEvent
  keyboardEvt <- getKeyboardEvent

  -- External triggers: fired from IO after the banner timer expires / for fade steps
  (resetEvt, fireReset) <- newTriggerEvent
  (alphaEvt, fireAlpha) <- newTriggerEvent
  (endTurnEvt, fireEndTurn) <- newTriggerEvent

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
      spaceEvt :: Event t ()
      spaceEvt = () <$ ffilter (\kd ->
          has _Pressed (keyboardEventKeyMotion kd) &&
          not (keyboardEventRepeat kd) &&
          keysymKeycode (keyboardEventKeysym kd) == KeycodeSpace
        ) keyboardEvt
      events = leftmost [ ShopUpdates <$> shopActions
                        , UseItem <$> inventoryActions
                        , LeftClick <$> leftClickAxial
                        , RightClick <$> rightClickAxialEvt
                        , Redraw <$ windowSizeChangedEvt
                        , Redraw <$ windowExposedEvt
                        , ToggleInventory <$ toggleInvEvt
                        , ResetGame <$ resetEvt
                        , EndTurn <$ endTurnEvt
                        , EndTurn <$ spaceEvt
                        ]
  performEvent_ $ ffor events $ liftIO . print

  ntDyn <- holdView makeRandomNT $ makeRandomNT <$ events
  state <- accumDyn updateState initialState ((,) <$> current ntDyn <@> events)

  phaseDyn <- holdUniqDyn (view game_phase <$> state)

  -- Fade-in alpha: step from 0→220 over 1 s (20 × 50 ms), then hold 4 s and reset.
  alphaDyn <- holdDyn 0 $ leftmost
    [ alphaEvt
    , 0 <$ ffilter (== Playing) (updated phaseDyn)
    ]
  -- When the phase transitions into a game-over state, animate the fade then reset.
  performEvent_ $ ffor (ffilter (/= Playing) (updated phaseDyn)) $ \_ ->
    liftIO $ void $ forkIO $ do
      forM_ [1..20 :: Int] $ \i -> do
        threadDelay 50000           -- 50 ms per step
        fireAlpha (fromIntegral (i * 11) `min` 220)
      threadDelay 4000000           -- hold for 4 s then reset
      fireReset ResetGame

  winSizeDyn <- holdDyn (V2 640 480) $
    fmap (fromIntegral <$>) (windowSizeChangedEventSize <$> windowSizeChangedEvt)

  performEvent_ $ ffor (describeState <$> updated state) $ liftIO . print

  pure (state, alphaDyn, winSizeDyn, fireEndTurn)
