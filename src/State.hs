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
import           Control.Monad
import           Control.Monad.Random.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Random.Lazy
import           Control.Monad.Trans.State.Lazy  hiding (put)
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Monoid
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

data Attack = MkAttackMove
  { _attack_move :: Move
  , _attack_to   :: Unit
  } deriving (Show, Eq)

data MoveType = MkWalk Move -- just go there (no additional events)
              | MkAttack Attack -- play out combat resolution
              deriving (Show, Eq)

data Move = MkMove
  { _move_from      :: Axial
  , _move_to        :: Axial
  , _move_from_unit :: Unit
  }
  deriving (Show, Eq, Generic)
makeLenses ''Move
makeLenses ''Attack
makePrisms ''MoveType

mTraverseBoard :: Axial -> Traversal' GameState (Maybe TileContent)
mTraverseBoard towards = game_board . at towards . _Just . tile_content

traverseBoard ::Axial -> Traversal' GameState TileContent
traverseBoard towards = mTraverseBoard towards . _Just

isAttack :: GameState -> Axial -> Maybe Unit
isAttack state' towards =
  preview (traverseBoard towards . _Enemy) state'

isMove :: GameState -> Axial -> Bool
isMove state' towards =
  has (mTraverseBoard towards . _Nothing) state'

-- figures out if the tile we're moving towards is a neigbour of the
-- selected tile, and verifies that hte selected tile is the player.
-- concatenates the given boolean to the the conditions.
toPlayerMove ::  GameState -> Axial -> Bool -> Maybe Move
toPlayerMove state' towards isMove' = do
  selectedAxial <- state' ^. game_selected
  selectedTile :: Tile  <- state' ^. game_board . at selectedAxial
  player :: Unit <- selectedTile ^? tile_content . _Just . _Player
  let
      shouldMove = and [ towards `elem`  neigbours selectedAxial
                       , isMove'
                       ]
  if shouldMove then
    pure $ MkMove { _move_from = selectedAxial
                  , _move_to = towards
                  , _move_from_unit = player
                  }
  else Nothing


-- this type is only moved if the target tile is free
shouldCharacterMove :: GameState -> Axial -> Maybe MoveType
shouldCharacterMove = over (mapped. mapped . mapped) MkWalk $
  getCompose $ Compose toPlayerMove <*> Compose isMove

-- if the target tile contains an enemy, it'll be this movetype
shouldCharacterAttack :: GameState -> Axial -> Maybe MoveType
shouldCharacterAttack state' axial = do
  attacking <- isAttack state' axial
  withMove <- toPlayerMove state' axial True
  pure $ MkAttack $ MkAttackMove
    { _attack_move = withMove
    , _attack_to = attacking
    }

move :: MoveType -> Grid -> Move -> Grid
move type' grid action =
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

figureOutMove :: MoveType -> Grid -> Grid
figureOutMove type' grid =
  maybe grid (move type' grid) action
  where
    action :: Maybe Move
    action = case type' of
      MkWalk move'   ->  Just move'
      MkAttack attack ->
        let
          toIsDead :: Maybe Bool
          toIsDead = isDead <$>
              grid ^? at (attack ^. attack_move . move_to) . _Just . tile_content . _Just . tc_unit . Combat.unit_hp
        in
        if toIsDead == Just True then
          Just (attack ^. attack_move)
          else
          Nothing


data UpdateEvts = LeftClick Axial
                | RightClick Axial
                deriving Show



applyAttack :: MonadRandom m => MonadState GameState m =>  MoveType -> m ()
applyAttack = \case
  MkAttack attack -> do
    result <- resolveCombat -- .... it's a maybe!
                (attack ^. attack_move . move_from_unit)
                (attack ^. attack_to)
    traverseBoard (attack ^. attack_move . move_from) . tc_unit .= fst result
    traverseBoard (attack ^. attack_move . move_to) . tc_unit .= snd result
    pure ()
  MkWalk _ -> pure ()


newtype RandTNT a = MkRandTNT {
  unRandNt :: forall n . Functor n => RandT StdGen n a -> n a
  }

updateLogic :: MonadRandom m => MonadState GameState m => UpdateEvts -> m ()
updateLogic = \case
  LeftClick axial -> assign game_selected (Just axial)
  RightClick towards -> do
    currentState <- use id
    let movePlan = shouldCharacterMove currentState towards
                    <|>
                    shouldCharacterAttack currentState towards
    traverse_ applyAttack movePlan
    for_ movePlan $ \plan -> modifying game_board (figureOutMove plan)


resetState :: MonadState GameState m => m ()
resetState = put initialState

checkPlayerLives :: MonadState GameState m => m ()
checkPlayerLives = do
  health <- preuse (game_board . folded . tile_content . _Just . _Player . unit_hp)
  when (maybe True isDead health) resetState



updateState :: GameState -> (RandTNT (), UpdateEvts) -> GameState
updateState gameState (resolveRng, evts) =
  execState (unRandNt resolveRng $ do
                updateLogic evts
                checkPlayerLives
            ) gameState
