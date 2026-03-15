{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Test host infrastructure that mirrors reflex-sdl2's @host@ function
--   but replaces the SDL event loop with programmatic event firing and
--   uses a mock 'RenderFun' that records draw calls to an 'IORef'.
module Test.TestHost
  ( RenderCall(..)
  , TestEnv(..)
  , TestHandle(..)
  , withTestEnv
  , bootApp
  ) where

import           Control.Concurrent       (Chan, newChan, newEmptyMVar, readChan)
import           Control.Concurrent.Async (async, cancel)
import           Control.Monad            (forM_)
import           Control.Monad.Identity   (Identity (..))
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Ref        (readRef)
import           Data.Dependent.Sum       (DSum ((:=>)))
import           Data.IORef
import           Data.Int                 (Int16)
import           Data.Word                (Word8)
import qualified Data.Vector.Storable     as S
import           Foreign.C.Types          (CInt)
import           GHC.Conc                 (TVar, atomically, newTVar, readTVar,
                                           writeTVar)
import           Reflex
import           Reflex.Host.Class
import           Reflex.SDL2
import           Reflex.SDL2.Base         (runReflexSDL2T)
import           Reflex.SDL2.Internal     (SystemEvents (..))
import qualified SDL.Font                 as Font
import           SDL.Image                (decodeTexture)
import           Control.Exception.Safe   (bracket)
import           System.Environment       (setEnv)

import           Plunder                  (app)
import           Plunder.Render.RenderFun (RenderFun (..))
import           Plunder.State            (GameState)

-- | Sum type capturing each render operation for test assertions.
--   Polygon vertex data is stored as plain lists (converted from
--   'S.Vector') so that expected values can be written as list literals.
data RenderCall
  = RcFillPolygon [Int16] [Int16] (V4 Word8)
  | RcPolygon     [Int16] [Int16] (V4 Word8)
  | RcFillRect    (Maybe (Rectangle CInt))
  | RcDrawRect    (Maybe (Rectangle CInt))
  | RcSetDrawColor (V4 Word8)
  | RcCopy        (Maybe (Rectangle CInt)) (Maybe (Rectangle CInt))
  | RcThickLine   (V2 CInt) (V2 CInt) CInt (V4 Word8)
  | RcFillTriangle (V2 CInt) (V2 CInt) (V2 CInt) (V4 Word8)
  | RcClear
  | RcPresent
  | RcSetRendererDrawColor (V4 Word8)
  deriving (Show, Eq)

-- | SDL resources created once per test suite.
data TestEnv = TestEnv
  { teWindow   :: Window
  , teRenderer :: Renderer
  }

-- | Handles for injecting events into the running reflex network.
data TestHandle = TestHandle
  { fireKeyboard          :: KeyboardEventData -> IO ()
  , fireMouseButton       :: MouseButtonEventData -> IO ()
  , fireWindowExposed     :: WindowExposedEventData -> IO ()
  , fireWindowSizeChanged :: WindowSizeChangedEventData -> IO ()
  , fireQuit              :: IO ()
  }

-- | Bracket that initialises SDL with the dummy video driver,
--   creates a window+renderer, runs the action, then tears down.
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
  setEnv "SDL_VIDEODRIVER" "dummy"
  initializeAll
  Font.initialize
  bracket acquire release $ \(w, r) -> do
    rendererDrawBlendMode r $= BlendAlphaBlend
    action (TestEnv w r)
  where
    acquire :: IO (Window, Renderer)
    acquire = do
      w <- createWindow "test" defaultWindow
      r <- createRenderer w (-1) defaultRenderer
      pure (w, r)
    release :: (Window, Renderer) -> IO ()
    release (w, r) = do
      destroyRenderer r
      destroyWindow w
      Font.quit
      quit

-- | Build a mock 'RenderFun' that records draw calls to the given 'IORef'
--   but delegates texture creation to the real renderer.
mkMockRenderFun :: IORef [RenderCall] -> Renderer -> RenderFun
mkMockRenderFun ref r = MkRenderFun
  { rf_fillRect      = \rect  -> liftIO $ modifyIORef' ref (++ [RcFillRect rect])
  , rf_drawRect      = \rect  -> liftIO $ modifyIORef' ref (++ [RcDrawRect rect])
  , rf_setDrawColor  = \c     -> liftIO $ modifyIORef' ref (++ [RcSetDrawColor c])
  , rf_copy          = \_ s d -> liftIO $ modifyIORef' ref (++ [RcCopy s d])
  , rf_fillPolygon   = \xs ys c -> liftIO $ modifyIORef' ref (++ [RcFillPolygon (S.toList xs) (S.toList ys) c])
  , rf_polygon       = \xs ys c -> liftIO $ modifyIORef' ref (++ [RcPolygon (S.toList xs) (S.toList ys) c])
  , rf_thickLine     = \a b w c -> liftIO $ modifyIORef' ref (++ [RcThickLine a b w c])
  , rf_fillTriangle  = \a b c col -> liftIO $ modifyIORef' ref (++ [RcFillTriangle a b c col])
  , rf_createTexture = createTextureFromSurface r
  , rf_decodeTexture = \bs -> decodeTexture r bs
  , rf_clear         = liftIO $ modifyIORef' ref (++ [RcClear])
  , rf_present       = liftIO $ modifyIORef' ref (++ [RcPresent])
  , rf_setRendererDrawColor = \c -> liftIO $ modifyIORef' ref (++ [RcSetRendererDrawColor c])
  }

-- | Boot the full app inside a reflex Spider host, returning handles
--   for programmatic event injection and the render-call log.
bootApp :: TestEnv -> GameState -> IO (TestHandle, IORef [RenderCall])
bootApp TestEnv{teRenderer} initGS = do
  renderCalls <- newIORef []
  let rf = mkMockRenderFun renderCalls teRenderer

  runSpiderHost $ do
    -- Create event triggers for all SystemEvents fields
    (sysPostBuildEvent,                 trPostBuildRef)             <- newEventWithTriggerRef
    (_sysAnySDLEvent,                   _trAnySDLRef)              <- newEventWithTriggerRef
    (_sysTicksEvent,                    _trTicksRef)               <- newEventWithTriggerRef
    (_sysWindowShownEvent,              _trWindowShownRef)         <- newEventWithTriggerRef
    (_sysWindowHiddenEvent,             _trWindowHiddenRef)        <- newEventWithTriggerRef
    (sysWindowExposedEvent,             trWindowExposedRef)        <- newEventWithTriggerRef
    (_sysWindowMovedEvent,              _trWindowMovedRef)         <- newEventWithTriggerRef
    (_sysWindowResizedEvent,            _trWindowResizedRef)       <- newEventWithTriggerRef
    (sysWindowSizeChangedEvent,         trWindowSizeChangedRef)    <- newEventWithTriggerRef
    (_sysWindowMinimizedEvent,          _trWindowMinimizedRef)     <- newEventWithTriggerRef
    (_sysWindowMaximizedEvent,          _trWindowMaximizedRef)     <- newEventWithTriggerRef
    (_sysWindowRestoredEvent,           _trWindowRestoredRef)      <- newEventWithTriggerRef
    (_sysWindowGainedMouseFocusEvent,   _trWindowGainedMouseFocusRef) <- newEventWithTriggerRef
    (_sysWindowLostMouseFocusEvent,     _trWindowLostMouseFocusRef)   <- newEventWithTriggerRef
    (_sysWindowGainedKeyboardFocusEvent, _trWindowGainedKeyboardFocusRef) <- newEventWithTriggerRef
    (_sysWindowLostKeyboardFocusEvent,  _trWindowLostKeyboardFocusRef) <- newEventWithTriggerRef
    (_sysWindowClosedEvent,             _trWindowClosedRef)        <- newEventWithTriggerRef
    (sysKeyboardEvent,                  trKeyboardRef)             <- newEventWithTriggerRef
    (_sysTextEditingEvent,              _trTextEditingRef)         <- newEventWithTriggerRef
    (_sysTextInputEvent,                _trTextInputRef)           <- newEventWithTriggerRef
    (_sysKeymapChangedEvent,            _trKeymapChangedRef)       <- newEventWithTriggerRef
    (_sysMouseMotionEvent,              _trMouseMotionRef)         <- newEventWithTriggerRef
    (sysMouseButtonEvent,               trMouseButtonRef)          <- newEventWithTriggerRef
    (_sysMouseWheelEvent,               _trMouseWheelRef)          <- newEventWithTriggerRef
    (_sysJoyAxisEvent,                  _trJoyAxisRef)             <- newEventWithTriggerRef
    (_sysJoyBallEvent,                  _trJoyBallRef)             <- newEventWithTriggerRef
    (_sysJoyHatEvent,                   _trJoyHatRef)              <- newEventWithTriggerRef
    (_sysJoyButtonEvent,                _trJoyButtonRef)           <- newEventWithTriggerRef
    (_sysJoyDeviceEvent,                _trJoyDeviceRef)           <- newEventWithTriggerRef
    (_sysControllerAxisEvent,           _trControllerAxisRef)      <- newEventWithTriggerRef
    (_sysControllerButtonEvent,         _trControllerButtonRef)    <- newEventWithTriggerRef
    (_sysControllerDeviceEvent,         _trControllerDeviceRef)    <- newEventWithTriggerRef
    (_sysAudioDeviceEvent,              _trAudioDeviceRef)         <- newEventWithTriggerRef
    (sysQuitEvent,                      trQuitRef)                 <- newEventWithTriggerRef
    (_sysUserEvent,                     _trUserRef)                <- newEventWithTriggerRef
    (_sysSysWMEvent,                    _trSysWMRef)               <- newEventWithTriggerRef
    (_sysTouchFingerEvent,              _trTouchFingerRef)         <- newEventWithTriggerRef
    (_sysTouchFingerMotionEvent,        _trTouchFingerMotionRef)   <- newEventWithTriggerRef
    (_sysMultiGestureEvent,             _trMultiGestureRef)        <- newEventWithTriggerRef
    (_sysDollarGestureEvent,            _trDollarGestureRef)       <- newEventWithTriggerRef
    (_sysDropEvent,                     _trDropRef)                <- newEventWithTriggerRef
    (_sysClipboardUpdateEvent,          _trClipboardUpdateRef)     <- newEventWithTriggerRef
    (_sysUnknownEvent,                  _trUnknownRef)             <- newEventWithTriggerRef

    -- Chan + TriggerEventT plumbing (mirrors host)
    chan <- liftIO newChan
    triggersVar <- liftIO $ atomically $ newTVar []
    sysQuitVar <- liftIO newEmptyMVar

    -- Async loop that drains the chan into triggersVar
    asyncTrigger <- liftIO $ async $ drainChan chan triggersVar

    let sysAnySDLEvent                    = _sysAnySDLEvent
        sysTicksEvent                     = _sysTicksEvent
        sysWindowShownEvent               = _sysWindowShownEvent
        sysWindowHiddenEvent              = _sysWindowHiddenEvent
        sysWindowMovedEvent               = _sysWindowMovedEvent
        sysWindowResizedEvent             = _sysWindowResizedEvent
        sysWindowMinimizedEvent           = _sysWindowMinimizedEvent
        sysWindowMaximizedEvent           = _sysWindowMaximizedEvent
        sysWindowRestoredEvent            = _sysWindowRestoredEvent
        sysWindowGainedMouseFocusEvent    = _sysWindowGainedMouseFocusEvent
        sysWindowLostMouseFocusEvent      = _sysWindowLostMouseFocusEvent
        sysWindowGainedKeyboardFocusEvent = _sysWindowGainedKeyboardFocusEvent
        sysWindowLostKeyboardFocusEvent   = _sysWindowLostKeyboardFocusEvent
        sysWindowClosedEvent              = _sysWindowClosedEvent
        sysTextEditingEvent               = _sysTextEditingEvent
        sysTextInputEvent                 = _sysTextInputEvent
        sysKeymapChangedEvent             = _sysKeymapChangedEvent
        sysMouseMotionEvent               = _sysMouseMotionEvent
        sysMouseWheelEvent                = _sysMouseWheelEvent
        sysJoyAxisEvent                   = _sysJoyAxisEvent
        sysJoyBallEvent                   = _sysJoyBallEvent
        sysJoyHatEvent                    = _sysJoyHatEvent
        sysJoyButtonEvent                 = _sysJoyButtonEvent
        sysJoyDeviceEvent                 = _sysJoyDeviceEvent
        sysControllerAxisEvent            = _sysControllerAxisEvent
        sysControllerButtonEvent          = _sysControllerButtonEvent
        sysControllerDeviceEvent          = _sysControllerDeviceEvent
        sysAudioDeviceEvent               = _sysAudioDeviceEvent
        sysUserEvent                      = _sysUserEvent
        sysSysWMEvent                     = _sysSysWMEvent
        sysTouchFingerEvent               = _sysTouchFingerEvent
        sysTouchFingerMotionEvent         = _sysTouchFingerMotionEvent
        sysMultiGestureEvent              = _sysMultiGestureEvent
        sysDollarGestureEvent             = _sysDollarGestureEvent
        sysDropEvent                      = _sysDropEvent
        sysClipboardUpdateEvent           = _sysClipboardUpdateEvent
        sysUnknownEvent                   = _sysUnknownEvent

    -- Build the network
    ((), FireCommand fire) <-
      hostPerformEventT $ flip runPostBuildT sysPostBuildEvent
                        $ flip runTriggerEventT chan
                        $ runReflexSDL2T (runReaderT (app initGS) rf) SystemEvents{..}

    -- Fire the post-build event
    (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
      fire [tr :=> Identity ()] $ return ()

    -- Drain any TriggerEventT events that the post-build may have enqueued
    liftIO $ drainTriggersOnce triggersVar

    -- Build IO handles that re-enter SpiderHost to fire events
    let handle = TestHandle
          { fireKeyboard = \dat -> runSpiderHost $ do
              mTr <- readRef trKeyboardRef
              forM_ mTr $ \tr -> fire [tr :=> Identity dat] $ return ()
              liftIO $ drainTriggersOnce triggersVar
          , fireMouseButton = \dat -> runSpiderHost $ do
              mTr <- readRef trMouseButtonRef
              forM_ mTr $ \tr -> fire [tr :=> Identity dat] $ return ()
              liftIO $ drainTriggersOnce triggersVar
          , fireWindowExposed = \dat -> runSpiderHost $ do
              mTr <- readRef trWindowExposedRef
              forM_ mTr $ \tr -> fire [tr :=> Identity dat] $ return ()
              liftIO $ drainTriggersOnce triggersVar
          , fireWindowSizeChanged = \dat -> runSpiderHost $ do
              mTr <- readRef trWindowSizeChangedRef
              forM_ mTr $ \tr -> fire [tr :=> Identity dat] $ return ()
              liftIO $ drainTriggersOnce triggersVar
          , fireQuit = do
              runSpiderHost $ do
                mTr <- readRef trQuitRef
                forM_ mTr $ \tr ->
                  fire [tr :=> Identity ()] $ return ()
              cancel asyncTrigger
          }

    pure (handle, renderCalls)

-- | Drain the TriggerEventT channel into the TVar (non-blocking).
drainChan :: Chan [DSum (EventTriggerRef Spider) TriggerInvocation]
          -> TVar [DSum (EventTriggerRef Spider) TriggerInvocation]
          -> IO ()
drainChan chan triggersVar = go
  where
    go = do
      trigs <- readChan chan
      atomically $ do
        prev <- readTVar triggersVar
        writeTVar triggersVar (prev ++ trigs)
      go

-- | Process any pending triggers from TriggerEventT (e.g. newTriggerEvent callbacks).
drainTriggersOnce :: TVar [DSum (EventTriggerRef Spider) TriggerInvocation] -> IO ()
drainTriggersOnce triggersVar = do
  triggers <- atomically $ do
    trigs <- readTVar triggersVar
    writeTVar triggersVar []
    return trigs
  forM_ triggers $ \(_ :=> TriggerInvocation _a cb) -> cb
