{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module State(GameState(..)
            , shouldCharacterMove
            , updateState
            , initialState
            , UpdateEvts(..)
            , game_board
            , game_selected
            , Move(..)
            ) where

import           Data.Foldable
import           Grid
import           Control.Lens
import Data.Monoid

data GameState = MkGameState
  { _game_selected :: Maybe Axial
  , _game_board    :: Grid
  } deriving Show
makeLenses ''GameState

level :: Grid -> Grid
level = fold
  [ at (MkAxial 2 3) . _Just . tile_content ?~ Player
  , at (MkAxial 4 5) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 4) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 3) . _Just . tile_content ?~ Enemy
  ]

initialState :: GameState
initialState = MkGameState Nothing $ level initialGrid

data Move = MkMove
  { _move_from :: Axial
  , _move_to   :: Axial
  } deriving (Show, Eq)
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
move action grid = fold
  [ at (action ^. move_from) .~ Nothing
  , at (action ^. move_to)   .~ (grid ^. at (action ^. move_from))
  ] grid

data UpdateEvts = LeftClick Axial
                | RightClick Axial
                deriving Show

updateState :: GameState -> UpdateEvts -> GameState
updateState state = \case
  LeftClick axial -> set game_selected (Just axial) state
  RightClick towards ->
    maybe state (\x -> over game_board (move x) state) $
      shouldCharacterMove state towards
