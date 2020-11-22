{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guest where

import           Control.Monad.Reader           ( MonadReader(..) )
import           Reflex
import           Reflex.SDL2
import           Layer
import           Hexagon
import           Data.Foldable
import           Grid
import           Control.Lens
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Int
import           Control.Monad

motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0 128
motionToColor Pressed  = V4 0 0 255 128

renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

leftClick :: Prism' MouseButton ()
leftClick = _Ctor @"ButtonLeft"

mouseButtons :: Lens' MouseButtonEventData MouseButton
mouseButtons = field @"mouseButtonEventButton"

mousePositions :: Lens' MouseButtonEventData (Point V2 Int32)
mousePositions = field @"mouseButtonEventPos"

guest
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m => MonadReader Renderer m => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB           <- getPostBuild
  mouseButtonEvt <- getMouseButtonEvent

  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."

  let evts :: Event t MouseButtonEventData
      evts = ffilter (has (mouseButtons . leftClick)) mouseButtonEvt
  void $ holdView (renderWithTile Nothing)
       $ renderWithTile .   Just .   selectedTile <$> evts

renderWithTile
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m => Maybe Tile -> m ()
renderWithTile _mtile = do
  liftIO $ putStrLn $ "selecitng " <> show _mtile
  traverse_ (hexagon . renderTile) $ unGrid initialGrid

selectedTile :: MouseButtonEventData -> Tile
selectedTile = pixelToTile . fmap fromIntegral . view mousePositions
