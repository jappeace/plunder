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

initialCharPos :: Tile
initialCharPos = Tile 2 3

data GameState = GameState
  { _game_selected :: Maybe Tile
  , _game_char_pos :: Tile
  } deriving Show
makeLenses ''GameState

initialState :: GameState
initialState = GameState Nothing initialCharPos

shouldCharacterMove :: GameState -> Tile -> Bool
shouldCharacterMove state towards = fromMaybe False $ do
  selected <- state ^. game_selected
  pure $ (selected == state ^. game_char_pos) &&
         towards `elem`  neigbours selected

updateState :: GameState -> Tile -> GameState
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
      leftCickedTile :: Event t Tile
      leftCickedTile = calcMouseClickTile <$> leftMouseClickEvts
      rightClickedTileEvt :: Event t Tile
      rightClickedTileEvt = calcMouseClickTile <$> rightMouseClickEvts

  calcMouseClickTileDyn <- holdDyn Nothing $ Just <$> leftCickedTile

  traverse_ (hexagon . renderTile) $ unGrid initialGrid

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> leftCickedTile

  viking <- loadViking

  performEvent_ $ ffor rightClickedTileEvt (\x -> liftIO $ print ("rightmouseclick", x))
  gameState <- mkGameState calcMouseClickTileDyn rightClickedTileEvt

  performEvent_ $ ffor (updated gameState) (liftIO . putStrLn . printf "gamestate %s" . show)
  image $ renderImage viking . view game_char_pos <$> gameState

mkGameState :: forall t m . ReflexSDL2 t m => Dynamic t (Maybe Tile) -> Event t Tile -> m (Dynamic t GameState)
mkGameState calcMouseClickTileDyn rightClickedTileEvt = do
  rec dynamicPlayerPos <- holdDyn initialState $ (current $ updateState <$> gameState) <@> rightClickedTileEvt
      let gameState :: Dynamic t GameState
          gameState = GameState <$> calcMouseClickTileDyn <*> (view game_char_pos <$> dynamicPlayerPos)
  pure dynamicPlayerPos

renderSelected :: Tile -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderTile

calcMouseClickTile :: MouseButtonEventData -> Tile
calcMouseClickTile = pixelToTile . fmap fromIntegral . view mousePositions
