{-# LANGUAGE DataKinds #-}
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

shouldCharacterMove :: Maybe Tile -> Tile -> Tile -> Bool
shouldCharacterMove Nothing _ _ = False
shouldCharacterMove (Just selected) charPos towards =
  if selected /= charPos then
    False
  else
    towards `elem`  neigbours selected

updateCharacter :: Maybe Tile -> Tile -> Tile -> Tile
updateCharacter mSelected charPos towards =
  if shouldCharacterMove mSelected towards charPos then
    towards
  else charPos

data GameState = GameState
  { game_selected :: Maybe Tile
  , game_char_pos :: Tile
  }

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

  performEvent_ $ ffor rightClickedTileEvt (\x -> liftIO $ print ("rightmouseclick", x))
  viking <- loadViking

  rec dynamicPlayerPos <- holdDyn initialCharPos $ (current $ updateCharacter <$> calcMouseClickTileDyn <*> dynamicPlayerPos) <@> rightClickedTileEvt
  performEvent_ $ ffor (updated dynamicPlayerPos) (\x -> liftIO $ print ("playerpos", x) )
  image $ renderImage viking <$> dynamicPlayerPos


renderSelected :: Tile -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderTile

calcMouseClickTile :: MouseButtonEventData -> Tile
calcMouseClickTile = pixelToTile . fmap fromIntegral . view mousePositions
