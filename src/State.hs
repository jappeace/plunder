{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module State(GameState(..)
            , shouldCharacterMove
            , updateState
            , initialState
            , UpdateEvts(..)
            , game_board
            , game_selected
            , Move(..)
            , MoveType(..)
            , describeState
            , move_from
            , move_to
            , move
            ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Monoid
import           Debug.Trace
import           GHC.Generics         (Generic)
import           Grid
import           Text.Printf

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

level :: Endo Grid
level = fold $ Endo <$>
  [ at (MkAxial 2 3) . _Just . tile_content ?~ Player
  , at (MkAxial 4 5) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 4) . _Just . tile_content ?~ Enemy
  , at (MkAxial 4 3) . _Just . tile_content ?~ Enemy
  , at (MkAxial 0 6) . _Just . tile_content ?~ Enemy
  , at (MkAxial 1 6) . _Just . tile_background ?~ Blood
  ]

initialState :: GameState
initialState = MkGameState Nothing $ appEndo level initialGrid


data MoveType = MkWalk Move -- just go there (no additional events)
              | MkAttack Move -- play out combat resolution
              deriving Show
data Move = MkMove
  { _move_from :: Axial
  , _move_to   :: Axial
  }
  deriving (Show, Eq, Generic)
makeLenses ''Move
makePrisms ''MoveType

isAttack :: GameState -> Axial -> Bool
isAttack state towards =
  has (game_board . at towards . _Just . tile_content . _Just . _Enemy) state

isMove :: GameState -> Axial -> Bool
isMove state towards =
  has (game_board . at towards . _Just . tile_content . _Nothing) state

isShould ::  GameState -> Axial -> Bool -> Maybe Move
isShould state towards isMove = do
  selectedAxial <- state ^. game_selected
  selectedTile :: Tile  <- state ^. game_board . at selectedAxial
  let shouldMove = and
                        [ has (tile_content . _Just . _Player)  selectedTile
                        , towards `elem`  neigbours selectedAxial
                        , isMove
                        ]
  if shouldMove then
    pure $ MkMove { _move_from = selectedAxial, _move_to = towards}
  else Nothing



shouldCharacterMove :: GameState -> Axial -> Maybe MoveType
shouldCharacterMove = over (mapped. mapped . mapped) MkWalk $
  getCompose $ Compose isShould <*> Compose isMove

shouldCharacterAttack :: GameState -> Axial -> Maybe MoveType
shouldCharacterAttack = over (mapped. mapped . mapped) MkAttack $
  getCompose $ Compose isShould <*> Compose isAttack

move :: MoveType -> Grid -> Grid
move type' grid =
  (toTileContent .~ (grid ^? fromTile . _Just)) $
  (if isAttack then (toTileBg ?~  Blood) else id) $
  (fromTile .~ Nothing) grid

  where
    fromTile :: Traversal' Grid (Maybe TileContent)
    fromTile = at (action ^. move_from) . _Just . tile_content

    toTileContent :: Traversal' Grid (Maybe TileContent)
    toTileContent =  toTile . tile_content

    toTileBg :: Traversal' Grid (Maybe Background)
    toTileBg =  toTile . tile_background

    toTile :: Traversal' Grid Tile
    toTile = ix (action ^. move_to)

    isAttack = has _MkAttack type'

    action :: Move
    action = case type' of
      MkWalk move ->  move
      MkAttack move ->  move

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
    result = maybe state (\x -> trace ("updating to" <> show x) $
                           over game_board (move x) state) $
      shouldCharacterMove state towards
      <|>
      shouldCharacterAttack state towards
    in trace ("result is now" <> describeState result) result

