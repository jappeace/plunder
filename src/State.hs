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
            , describeState
            , move_from
            , move_to
            , move
            ) where

import           Data.Foldable
import           Grid
import           Control.Lens
import Data.Monoid
import Text.Printf
import Debug.Trace
import           GHC.Generics    (Generic)

data GameState = MkGameState
  { _game_selected :: Maybe Axial
  , _game_board    :: Grid
  } deriving (Show)
makeLenses ''GameState

-- filters out irrelevant stuff
describeState :: GameState -> String
describeState x = printf "GameState { _game_selected = %s, _game_board = %s }"
  (show (x ^. game_selected))
  (show (x ^.. game_board . contentFold))

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
  } deriving (Show, Eq, Generic)
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
  [ toTile .~ (grid ^? fromTile . _Just)
  , fromTile .~ Nothing
  ] grid

  where
    fromTile :: Traversal' Grid (Maybe TileContent)
    fromTile = at (action ^. move_from) . _Just . tile_content

    toTile :: Traversal' Grid (Maybe TileContent)
    toTile = at (action ^. move_to)  . _Just . tile_content

data UpdateEvts = LeftClick Axial
                | RightClick Axial
                deriving Show

-- TODO add logging:
--  https://hackage.haskell.org/package/reflex-0.8.0.0/docs/Reflex-Class.html#v:mapAccumDyn
--  https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:MonadLogger
--  https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:WriterLoggingT
updateState :: GameState -> UpdateEvts -> GameState
updateState state = \case
  LeftClick axial -> set game_selected (Just axial) state
  RightClick towards -> let
    result = maybe state (\x -> trace ("updating to" <> show x) $ over game_board (move x) state) $
      shouldCharacterMove state towards
    in trace ("result is now" <> describeState result) result
