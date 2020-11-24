{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
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
import Image
import Data.Maybe
import Text.Printf

motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0 128
motionToColor Pressed  = V4 0 0 255 128

renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

leftClick :: Prism' MouseButton ()
leftClick = _Ctor @"ButtonLeft"

rightClick :: Prism' MouseButton ()
rightClick = _Ctor @"ButtonRight"

mouseButtons :: Lens' MouseButtonEventData MouseButton
mouseButtons = field @"mouseButtonEventButton"

mousePositions :: Lens' MouseButtonEventData (Point V2 Int32)
mousePositions = field @"mouseButtonEventPos"

initialCharPos :: Axial
initialCharPos = MkAxial 2 3

data GameState = GameState
  { _game_selected :: Maybe Axial
  , _game_char_pos :: Axial
  } deriving Show
makeLenses ''GameState

initialState :: GameState
initialState = GameState Nothing initialCharPos

shouldCharacterMove :: GameState -> Axial -> Bool
shouldCharacterMove state towards = fromMaybe False $ do
  selected <- state ^. game_selected
  pure $ (selected == state ^. game_char_pos) &&
         towards `elem`  neigbours selected

updateState :: GameState -> Axial -> GameState
updateState state towards =
  if shouldCharacterMove state towards then
    game_char_pos .~ towards $ state
  else state


guest
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m => MonadReader Renderer m => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB           <- getPostBuild
  mouseButtonEvt <- getMouseButtonEvent

  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."

  let leftMouseClickEvts :: Event t MouseButtonEventData
      leftMouseClickEvts = ffilter (has (mouseButtons . leftClick)) mouseButtonEvt
      rightMouseClickEvts :: Event t MouseButtonEventData
      rightMouseClickEvts = ffilter (has (mouseButtons . rightClick)) mouseButtonEvt
      leftCickedAxial :: Event t Axial
      leftCickedAxial = calcMouseClickAxial <$> leftMouseClickEvts
      rightClickedAxialEvt :: Event t Axial
      rightClickedAxialEvt = calcMouseClickAxial <$> rightMouseClickEvts

  calcMouseClickAxialDyn <- holdDyn Nothing $ Just <$> leftCickedAxial

  traverse_ (hexagon . renderHex) $ unGrid initialGrid

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> leftCickedAxial

  viking <- loadViking

  performEvent_ $ ffor rightClickedAxialEvt (\x -> liftIO $ putStrLn $ printf "rightmouseclick %s" (show x))
  gameState <- mkGameState calcMouseClickAxialDyn rightClickedAxialEvt

  performEvent_ $ ffor (updated gameState) (liftIO . putStrLn . printf "gamestate %s" . show)
  image $ renderImage viking . view game_char_pos <$> gameState

mkGameState :: forall t m . ReflexSDL2 t m => Dynamic t (Maybe Axial) -> Event t Axial -> m (Dynamic t GameState)
mkGameState calcMouseClickAxialDyn rightClickedAxialEvt = do
  rec dynamicPlayerPos <- holdDyn initialState $ (current $ updateState <$> gameState) <@> rightClickedAxialEvt
      let gameState :: Dynamic t GameState
          gameState = GameState <$> calcMouseClickAxialDyn <*> (view game_char_pos <$> dynamicPlayerPos)
  pure dynamicPlayerPos

renderSelected :: Axial -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex

calcMouseClickAxial :: MouseButtonEventData -> Axial
calcMouseClickAxial = pixelToAxial . fmap fromIntegral . view mousePositions
