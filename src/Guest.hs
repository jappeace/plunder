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

data Move = MkMove
  { _move_from :: Axial
  , _move_to   :: Axial
  }
makeLenses ''Move

shouldCharacterMove :: GameState -> Axial -> Maybe Move
shouldCharacterMove state towards = do
  selectedAxial <- state ^. game_selected
  selectedTile :: Tile  <- state ^. game_board . at selectedAxial
  let shouldMove = fold $ All <$>
                        [ has (tile_content . _Just . _Player)  selectedTile
                        , towards `elem`  neigbours selectedAxial
                        , has (game_board . at towards . _Just . tile_content . _Nothing) state
                        ]
  if getAll shouldMove then
    pure $ MkMove { _move_from = selectedAxial, _move_to = towards}
  else Nothing

move :: Move -> Grid -> Grid
move move grid = fold
  [ at (move ^. move_from) .~ Nothing
  , at (move ^. move_to)   .~ (grid ^. at (move ^. move_from))
  ] grid

data UpdateEvts = LeftClick Axial
                | RightClick Axial

updateState :: GameState -> UpdateEvts -> GameState
updateState state = \case
  LeftClick axial -> set game_selected (Just axial) state
  RightClick towards -> maybe state (\x -> over game_board (move x) state) $ shouldCharacterMove state towards


guest
  :: forall t m
   . ReflexSDL2 t m
  => DynamicWriter t [Layer m] m => MonadReader Renderer m => m ()
guest = do
  -- Print some stuff after the network is built.
  evPB           <- getPostBuild
  mouseButtonEvt <- getMouseButtonEvent

  performEvent_ $ ffor evPB $ \() -> liftIO $ putStrLn "starting up..."

  let leftClickEvts :: Event t MouseButtonEventData
      leftClickEvts = ffilter (has (mouseButtons . leftClick)) mouseButtonEvt
      rightClickEvts :: Event t MouseButtonEventData
      rightClickEvts = ffilter (has (mouseButtons . rightClick)) mouseButtonEvt
      leftClickAxial = calcMouseClickAxial <$> leftClickEvts
      rightClickAxialEvt = calcMouseClickAxial <$> rightClickEvts

  viking <- loadViking

  performEvent_ $ ffor rightClickAxialEvt (\x -> liftIO $ putStrLn $ printf "rightmouseclick %s" (show x))
  gameState <- mkGameState $ leftmost [ LeftClick <$> leftClickAxial
                                      , RightClick <$> rightClickAxialEvt
                                      ]
  renderState gameState
  void $ holdView (pure ())
       $ hexagon . renderSelected <$> leftClickAxial


renderTile :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Tile -> m ()

renderTile :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Tile -> m ()
renderTile _tile = pure ()


mkGameState :: forall t m . ReflexSDL2 t m => Event t UpdateEvts -> m (Dynamic t GameState)
mkGameState events = accum updateState initialState events

renderSelected :: Axial -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex

calcMouseClickAxial :: MouseButtonEventData -> Axial
calcMouseClickAxial = pixelToAxial . fmap fromIntegral . view mousePositions
