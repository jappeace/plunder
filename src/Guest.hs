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
import Data.Monoid

leftClick :: Prism' MouseButton ()
leftClick = _Ctor @"ButtonLeft"

rightClick :: Prism' MouseButton ()
rightClick = _Ctor @"ButtonRight"

mouseButtons :: Lens' MouseButtonEventData MouseButton
mouseButtons = field @"mouseButtonEventButton"

mousePositions :: Lens' MouseButtonEventData (Point V2 Int32)
mousePositions = field @"mouseButtonEventPos"

data GameState = GameState
  { _game_selected :: Maybe Axial
  , _game_board    :: Grid
  } deriving Show
makeLenses ''GameState

level :: Endo Grid
level = fold $ Endo <$>
  [ at (MkAxial 2 3) . _Just . tile_content ?~ Player
  , at (MkAxial 4 5) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 4) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 3) . _Just . tile_content ?~ Enemy
  ]

initialState :: GameState
initialState = GameState Nothing $ appEndo level initialGrid

shouldCharacterMove :: GameState -> Axial -> Bool
shouldCharacterMove state towards = fromMaybe False $ do
  selectedAxial <- state ^. game_selected
  selectedTile <- state ^? game_board . at selectedAxial
  pure $ fold $ All <$> [ has (_Just . _Player)  selectedTile
                        , towards `elem`  neigbours selectedAxial
                        , has (game_board . at towards . _Just . tile_content . _Nothing) state
                        ]

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

  traverse_ (hexagon . renderHex . view tile_axial) initialGrid

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> leftCickedAxial

  viking <- loadViking

  performEvent_ $ ffor rightClickedAxialEvt (\x -> liftIO $ putStrLn $ printf "rightmouseclick %s" (show x))
  gameState <- mkGameState calcMouseClickAxialDyn rightClickedAxialEvt

  performEvent_ $ ffor (updated gameState) (liftIO . putStrLn . printf "gamestate %s" . show)
  image $ renderImage viking . view game_char_pos <$> gameState

renderTile :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Tile -> m ()
renderTile _tile = pure ()

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
