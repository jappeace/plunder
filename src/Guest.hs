{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Guest where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2
import Button
import Layer

data AABB = AABB InputMotion (V2 Int)

mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32

motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128

renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

guest
  :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
  => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  ------------------------------------------------------------------------------
  -- Get a handle on our renderer
  ------------------------------------------------------------------------------
  r <- ask
  ------------------------------------------------------------------------------
  -- Test async events.
  -- This will wait three seconds before coloring the background black.
  ------------------------------------------------------------------------------
  evDelay <- getAsyncEvent $ threadDelay 3000000
  dDelay  <- holdDyn False $ True <$ evDelay
  commitLayers $ ffor dDelay $ \case
    False -> pure $ do
      rendererDrawColor r $= V4 128 128 128 255
      fillRect r Nothing
    True  -> pure $ do
      rendererDrawColor r $= V4 0 0 0 255
      fillRect r Nothing

  ------------------------------------------------------------------------------
  -- A button!
  ------------------------------------------------------------------------------
  evBtnState <- button
  let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
  performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button pressed!"

  ------------------------------------------------------------------------------
  -- Ghosty trail of squares
  ------------------------------------------------------------------------------
  -- Gather all mouse motion events into a list, then commit a commitLayers that
  -- renders each move as a quarter alpha'd yello or cyan square.
  evMouseMove <- getMouseMotionEvent
  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  commitLayer $ ffor dMoves $ \moves ->
    forM_ (reverse moves) $ \dat -> do
      let P pos = fromIntegral <$> mouseMotionEventPos dat
          color = if null (mouseMotionEventState dat)
                  then V4 255 255 0   128
                  else V4 0   255 255 128
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- Up and down squares
  ------------------------------------------------------------------------------
  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Commit a commitLayers of those rendered up/down AABBs.
  evMouseButton <- getMouseButtonEvent
  dBtns         <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseButton
  commitLayer $ ffor dBtns $ \btns ->
    forM_ (reverse btns) $ \dat -> do
      let AABB motion pos = mouseButtonToAABB dat
          color = motionToColor motion
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- An ephemeral commitLayers that only renders when a key is down, and only listens
  -- to the tick event while that key is down.
  -- This is an example of the higher-order nature of the reflex network. We
  -- can update the shape of the network in response to events within it.
  ------------------------------------------------------------------------------
  evKey <- getKeyboardEvent
  let evKeyNoRepeat = fmapMaybe (\k -> k <$ guard (not $ keyboardEventRepeat k)) evKey
  dPressed <- holdDyn False $ (== Pressed) . keyboardEventKeyMotion <$> evKeyNoRepeat
  void $ holdView (return ()) $ ffor (updated dPressed) $ \case
    False -> return ()
    True  -> do
      evDeltaTick <- getDeltaTickEvent
      dTimePressed <- foldDyn (+) 0 evDeltaTick
      commitLayer $ ffor dTimePressed $ \t -> do
        let wrap :: Float -> Int
            wrap x = if x > 255 then wrap (x - 255) else floor x
            rc    = wrap $ fromIntegral t/1000 * 255
            gc    = wrap $ fromIntegral t/2000 * 255
            bc    = wrap $ fromIntegral t/3000 * 255
            color :: V4 Int
            color = fromIntegral <$> V4 rc gc bc 255
        renderAABB r color 100

  ------------------------------------------------------------------------------
  -- Test our recurring timer events
  ------------------------------------------------------------------------------
  let performDeltaSecondTimer n = do
        evDelta  <- performEventDelta =<< tickLossyFromPostBuildTime n
        dTicks   <- foldDyn (+) 0 $ (1 :: Int) <$ evDelta
        dDelta   <- holdDyn 0 evDelta
        dElapsed <- foldDyn (+) 0 evDelta
        flip putDebugLnE id $ updated $ do
          tickz <- dTicks
          lapse <- dElapsed
          delta <- dDelta
          return $ unwords [ show n
                           , "timer -"
                           , show tickz
                           , "ticks -"
                           , show lapse
                           , "lapsed -"
                           , show delta
                           , "delta since last tick"
                           ]
  performDeltaSecondTimer 1

  ------------------------------------------------------------------------------
  -- Quit on a quit event
  ------------------------------------------------------------------------------
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit
