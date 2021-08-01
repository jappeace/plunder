{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
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
            , RandTNT(..)
            ) where

import           Combat
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Random.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Random.Lazy
import           Control.Monad.Trans.State.Lazy
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           GHC.Generics                    (Generic)
import           Grid
import           System.Random
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
  [ at (MkAxial 2 3) . _Just . tile_content ?~ Player (unit_weapon ?~ Axe $ defUnit)
  , at (MkAxial 4 5) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Axe $ defUnit)
  , at (MkAxial 4 4) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Bow $ defUnit)
  , at (MkAxial 4 3) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Sword $ defUnit)
  , at (MkAxial 0 6) . _Just . tile_content ?~ Enemy defUnit
  , at (MkAxial 1 6) . _Just . tile_background ?~ Blood
  ]

initialState :: GameState
initialState = MkGameState Nothing $ appEndo level initialGrid


data MoveType = MkWalk Move -- just go there (no additional events)
              | MkAttack Move -- play out combat resolution
              deriving (Show, Eq)
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

-- figures out if the tile we're moving towards is a neigbour of the
-- selected tile, and verifies that hte selected tile is the player.
-- concatenates the given boolean to the the conditions.
toPlayerMove ::  GameState -> Axial -> Bool -> Maybe Move
toPlayerMove state towards isMove' = do
  selectedAxial <- state ^. game_selected
  selectedTile :: Tile  <- state ^. game_board . at selectedAxial
  let shouldMove = and
                        [ has (tile_content . _Just . _Player)  selectedTile
                        , towards `elem`  neigbours selectedAxial
                        , isMove'
                        ]
  if shouldMove then
    pure $ MkMove { _move_from = selectedAxial, _move_to = towards}
  else Nothing


-- this type is only moved if the target tile is free
shouldCharacterMove :: GameState -> Axial -> Maybe MoveType
shouldCharacterMove = over (mapped. mapped . mapped) MkWalk $
  getCompose $ Compose toPlayerMove <*> Compose isMove

-- if the target tile contains an enemy, it'll be this movetype
shouldCharacterAttack :: GameState -> Axial -> Maybe MoveType
shouldCharacterAttack = over (mapped. mapped . mapped) MkAttack $
  getCompose $ Compose toPlayerMove <*> Compose isAttack

move :: MoveType -> Grid -> Grid
move type' grid =
  (toTileContent .~ (grid ^? fromTile . _Just)) $
  (if isAttack' then (toTileBg ?~  Blood) else id) $
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

    isAttack' = has _MkAttack type'

    action :: Move
    action = case type' of
      MkWalk move'   ->  move'
      MkAttack move' ->  move'

data UpdateEvts = LeftClick Axial
                | RightClick Axial
                deriving Show



applyAttack :: MonadRandom m => MonadState GameState m =>  MoveType -> m ()
applyAttack = \case
  MkAttack move -> do
    mFrom <- preuse $ game_board . ix (move ^. move_from)
    mTo <- preuse $ game_board . ix (move ^. move_to)
    let
        from :: Tile -- TODO figure out how to structure that this maybe isn't neccisary
        from = fromMaybe (error "could not find from tile were expected") $ mFrom
        to :: Tile -- TODO figure out how to structure that this maybe isn't neccisary
        to = fromMaybe (error "could not find to tile were expected") $ mTo
        fromU :: Unit -- TODO figure out how to structure that this maybe isn't neccisary
        fromU = fromMaybe (error "from no unit found where expected") $
                          (from ^? tile_content . _Just . tc_unit)

        toU :: Unit -- TODO figure out how to structure that this maybe isn't neccisary
        toU = fromMaybe (error "to no unit found where expected") $
                          (to ^? tile_content . _Just . tc_unit)
    result <- resolveCombat -- .... it's a maybe!
                fromU
                toU
    game_board . at (move ^. move_from) . _Just . tile_content . _Just . tc_unit .= fst result
    game_board . at (move ^. move_to) . _Just  . tile_content . _Just . tc_unit .= snd result
    pure ()
  MkWalk x -> pure ()


newtype RandTNT a = MkRandTNT {
  unRandNt :: forall n . Functor n => RandT StdGen n a -> n a
  }

-- TODO add logging:
--  https://hackage.haskell.org/package/reflex-0.8.0.0/docs/Reflex-Class.html#v:mapAccumDyn
--  https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:MonadLogger
--  https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:WriterLoggingT
updateState :: GameState -> (RandTNT (), UpdateEvts) -> GameState
updateState state (resolveRng, evts) = case evts of
  LeftClick axial -> set game_selected (Just axial) state
  RightClick towards -> let
    movePlan =
      shouldCharacterMove state towards
      <|>
      shouldCharacterAttack state towards

    newState = fromMaybe state $ movePlan <&> \plan -> execState (unRandNt resolveRng $ applyAttack plan) state

    movedState = maybe newState (\x -> trace ("updating to" <> show x) $
                           over game_board (move x) newState) movePlan
    in trace ("result is now" <> describeState movedState) movedState
