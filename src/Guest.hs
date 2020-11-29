{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Guest(guest) where

import           Control.Monad.Reader           ( MonadReader(..) )
import           Reflex
import           Reflex.SDL2
import           Layer
import           Hexagon
import           Grid
import           Control.Lens
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Int
import           Control.Monad
import Image
import Data.Bool
import State
import Data.Functor.Compose

leftClick :: Prism' MouseButton ()
leftClick = _Ctor @"ButtonLeft"

rightClick :: Prism' MouseButton ()
rightClick = _Ctor @"ButtonRight"

mouseButtons :: Lens' MouseButtonEventData MouseButton
mouseButtons = field @"mouseButtonEventButton"

mousePositions :: Lens' MouseButtonEventData (Point V2 Int32)
mousePositions = field @"mouseButtonEventPos"

mouseMotion :: Lens' MouseButtonEventData InputMotion
mouseMotion = field @"mouseButtonEventMotion"

_Pressed :: Prism' InputMotion ()
_Pressed = _Ctor @ "Pressed"

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

renderState :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Dynamic t GameState -> m ()
renderState state = do
  vikingF <- renderImage <$> loadViking
  void $ listWithKey (view game_board <$> state) $ \axial _ -> do
    hexagon $ renderHex axial

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> mapMaybe (view game_selected) (updated state)

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn -> do
    let playerSettings = bool Nothing (Just axial)
                       . has (tile_content . _Just . _Player) <$> tileDyn
    performEvent_ $ ffor (updated playerSettings) (maybe (pure ()) $ liftIO . print)

    image $ getCompose $ vikingF <$> Compose playerSettings

mkGameState :: forall t m . ReflexSDL2 t m => m (Dynamic t GameState)
mkGameState = do
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
                        ]
  performEvent_ $ ffor events $ liftIO . print
  state <- accumDyn updateState initialState events
  performEvent_ $ ffor (describeState <$> updated state) $ liftIO . print
  pure state

renderSelected :: Axial -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex

calcMouseClickAxial :: MouseButtonEventData -> Axial
calcMouseClickAxial = pixelToAxial . fmap fromIntegral . view mousePositions
