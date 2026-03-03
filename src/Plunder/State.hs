{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Elm architecture around gamestate
module Plunder.State(GameState(..)
            , shouldCharacterMove
            , updateState
            , initialState
            , UpdateEvts(..)
            , game_board
            , game_selected
            , game_player_inventory
            , game_shop
            , game_inventory_open
            , game_phase
            , game_planned_moves
            , inventory_money
            , inventroy_item
            , PlayerInventory(..)
            , Move(..)
            , Action(..)
            , GamePhase(..)
            , describeState
            , move_from
            , move_to
            , move
            , RandTNT(..)
            , findFreeAdjacent
            ) where

import qualified Control.Monad.State.Class as SC
import           Plunder.Combat
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
import           Data.Word
import           Debug.Trace
import           GHC.Generics                    (Generic)
import           Plunder.Grid
import           System.Random
import           Text.Printf
import Plunder.Shop
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(listToMaybe)

data GamePhase = Playing | YouDied | YouVictorious deriving (Show, Eq)

-- | inidicates the stuff in "pockets", so this doesn't mean equiped
--   equiped is handled by tile content.
data PlayerInventory = MkInventory {
   -- | indicating how much money a player has
    _inventory_money :: Word64
  , _inventroy_item  :: Set ShopItem
  } deriving (Show)

data GameState = MkGameState
  { _game_selected      :: Maybe Axial
  , _game_board         :: Grid
  , _game_player_inventory :: PlayerInventory
  , _game_shop          :: Maybe ShopContent -- If just we're at the shopping screen
  , _game_inventory_open :: Bool
  , _game_phase         :: GamePhase
  , _game_planned_moves :: Map Axial Axial   -- ^ from -> to: queued player moves
  } deriving (Show)
makeLenses ''GameState
makeLenses ''PlayerInventory

-- filters out irrelevant stuff
describeState :: GameState -> String
describeState x = printf "describeState { _game_selected = %s, _game_shop = %s }"
  (show (x ^. game_selected))
  (show (x ^. game_shop))
  -- (show (x ^.. game_board . contentFold))

level :: Endo Grid
level = fold $ Endo <$>
  [ at (MkAxial 2 3) . _Just . tile_content ?~ Player (unit_weapon ?~ Axe $ defUnit)
  , at (MkAxial 3 3) . _Just . tile_content ?~ House defUnit
  , at (MkAxial 2 6) . _Just . tile_content ?~ Shop (MkShopContent (Just (MkShopItem 4 ShopHealthPotion)) (Just (MkShopItem 8 ShopUnit)) Nothing)
  , at (MkAxial 4 5) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Axe $ defUnit)
  , at (MkAxial 4 4) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Bow $ defUnit)
  , at (MkAxial 4 3) . _Just . tile_content ?~ Enemy (unit_weapon ?~ Sword $ defUnit)
  , at (MkAxial 0 6) . _Just . tile_content ?~ Enemy defUnit
  , at (MkAxial 1 6) . _Just . tile_background ?~ Blood
  ]

allEnemies :: Traversal' GameState Unit
allEnemies = game_board . traversed . tile_content . _Just . _Enemy

initialInventory :: PlayerInventory
initialInventory = MkInventory
  { _inventory_money = 0
  , _inventroy_item  = mempty
  }

initialState :: GameState
initialState = MkGameState
  { _game_selected      = Nothing
  , _game_board         = appEndo level initialGrid
   -- indicating how much havoc a player caused,
   -- potentially we could use this later as currency?
  , _game_player_inventory = initialInventory
  , _game_shop          = Nothing
  , _game_inventory_open = False
  , _game_phase         = Playing
  , _game_planned_moves = Map.empty
  }

data Attack = MkAttackMove
  { _attack_move :: Move
  , _attack_to   :: TileContent
  } deriving (Show, Eq)

data Action = MkWalk Move -- ^ just go there (no additional events)
              | MkAttack Attack -- ^ play out combat resolution
              | OpenShop ShopContent-- ^ Open shop screen
              deriving (Show, Eq)

data Move = MkMove
  { _move_from      :: Axial
  , _move_to        :: Axial
  , _move_from_unit :: Unit
  }
  deriving (Show, Eq, Generic)

makeLenses ''Move
makeLenses ''Attack
makePrisms ''Action

mTraverseBoard :: Axial -> Traversal' GameState (Maybe TileContent)
mTraverseBoard towards = game_board . at towards . _Just . tile_content

traverseBoard ::Axial -> Traversal' GameState TileContent
traverseBoard towards = mTraverseBoard towards . _Just

isAttack :: GameState -> Axial -> Maybe TileContent
isAttack state' towards =
  preview (traverseBoard towards) state'

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
  let shouldMove = and [ towards `elem`  neigbours selectedAxial
                       , isMove'
                       ]
  if shouldMove then
    pure $ MkMove { _move_from = selectedAxial
                  , _move_to = towards
                  , _move_from_unit = player
                  }
  else Nothing

shouldCharacterMove :: GameState -> Axial -> Maybe Action
shouldCharacterMove = over (mapped. mapped . mapped) MkWalk $
  getCompose $ Compose toPlayerMove <*> Compose isMove

isShopping :: GameState -> Axial -> Maybe Action
isShopping currentState towards = do
  content <- preview (traverseBoard towards . _Shop) currentState
  -- Require the player to be selected and adjacent (same adjacency check as a move,
  -- but passing True for isMove since the shop tile is not empty)
  _ <- toPlayerMove currentState towards True
  pure (OpenShop content)

shouldCharacterAttack :: GameState -> Axial -> Maybe Action
shouldCharacterAttack state' towards = do
  attacking <- isAttack state' towards
  withMove <- toPlayerMove state' towards True
  pure $ MkAttack $ MkAttackMove
    { _attack_move = withMove
    , _attack_to = attacking
    }

move :: Action -> Grid -> Move -> Grid
move type' grid action =
  (toTileContent .~ (grid ^? fromTile . _Just)) $
  toTileBg .~ background $
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

    background :: Maybe Background
    background = do
      content' <- preview (_MkAttack . attack_to) type'
      pure $ case content' of
        Player _ -> Blood
        Enemy _ -> Blood
        House _ -> BurnedHouse
        Shop  _ -> BurnedShop

figureOutMove :: Maybe Result -> Action -> Grid -> Grid
figureOutMove res type' grid =
  maybe grid (move type' grid) action
  where
    action :: Maybe Move
    action = case type' of
      MkWalk move'   ->  Just move'
      MkAttack attack ->
        if (isTargetDead <$> res) == Just True then
          Just (attack ^. attack_move)
          else
          Nothing
      OpenShop _ -> Nothing

data UpdateEvts = LeftClick Axial
                | RightClick Axial
                | Redraw -- ^ eg window size changed, needs an update
                | ShopUpdates ShopAction
                | ToggleInventory
                | ResetGame -- ^ fired after the death/victory banner expires
                | EndTurn   -- ^ execute all planned moves
                deriving Show

applyAttack :: MonadRandom m => MonadState GameState m =>  Action -> m (Maybe Result)
applyAttack = \case
  MkAttack attack -> case attack ^? attack_to . tc_unit of
    Nothing -> pure Nothing  -- if tc has no unit, it's not attackable
    Just attacking -> do
      result <- resolveCombat
                  (attack ^. attack_move . move_from_unit)
                  attacking
      traverseBoard (attack ^. attack_move . move_from) . tc_unit .= (result ^. res_left_unit)
      traverseBoard (attack ^. attack_move . move_to) . tc_unit .= (result ^. res_right_unit)
      pure (Just result)
  MkWalk _ -> pure Nothing
  OpenShop _ -> pure Nothing

newtype RandTNT a = MkRandTNT {
  unRandNt :: forall n . Functor n => RandT StdGen n a -> n a
  }

countLoot :: MonadState GameState m => Action -> Result -> m ()
countLoot plan res =
  when (isTargetDead res) $
   when (has (_MkAttack . attack_to . _House) plan) $
         game_player_inventory . inventory_money += 10

-- | If a Player is selected, allow planning a move to an adjacent empty tile
--   (or to the unit's own tile to cancel the plan).
planPlayerMove :: GameState -> Axial -> Maybe (Axial, Axial)
planPlayerMove state' towards = do
  selectedAxial <- state' ^. game_selected
  _player :: Unit <- state' ^? game_board . ix selectedAxial . tile_content . _Just . _Player
  guard $ towards == selectedAxial
       || (towards `elem` neigbours selectedAxial && isMove state' towards)
  pure (selectedAxial, towards)

-- | Re-derive a walk action at execution time so that moves that became
--   invalid (destination taken) are silently skipped.
executePlannedMove :: GameState -> Axial -> Axial -> Maybe Action
executePlannedMove gs src dst = do
  unit' <- gs ^? game_board . ix src . tile_content . _Just . _Player
  guard (isMove gs dst)
  pure $ MkWalk $ MkMove { _move_from = src, _move_to = dst, _move_from_unit = unit' }

updateLogic :: MonadRandom m => MonadState GameState m => UpdateEvts -> m ()
updateLogic = \case
  Redraw -> pure ()
  ToggleInventory -> modifying game_inventory_open not
  ShopUpdates actions -> applyShopUpdates actions
  LeftClick axial -> assign game_selected (Just axial)
  ResetGame -> put initialState
  EndTurn -> do
    plans <- use game_planned_moves
    game_planned_moves .= Map.empty
    for_ (Map.toList plans) $ \(src, dst) -> do
      gs <- use id
      for_ (executePlannedMove gs src dst) $ \plan ->
        modifying game_board (figureOutMove Nothing plan)
    game_selected .= Nothing
  RightClick towards -> do
    currentState <- use id
    -- Shopping and attacks remain immediate; only empty-tile walks are queued.
    let immediateAction = isShopping currentState towards
                           <|> shouldCharacterAttack currentState towards
    case immediateAction of
      Just plan -> trace (show plan) $ do
        mCombatRes <- applyAttack plan
        traverse_ (countLoot plan) mCombatRes
        modifying game_board (figureOutMove mCombatRes plan)
        traverse_ applyShop (plan ^? _OpenShop)
      Nothing ->
        for_ (planPlayerMove currentState towards) $ \(src, dst) ->
          if src == dst
            then game_planned_moves . at src .= Nothing   -- cancel
            else game_planned_moves . at src ?= dst        -- plan or overwrite

applyShop :: MonadState GameState m => ShopContent -> m ()
applyShop content = assign game_shop (Just content)

-- | Find a free tile adjacent to any Player unit, used to decide where to
--   spawn a purchased friend and whether buying one is currently allowed.
findFreeAdjacent :: GameState -> Maybe Axial
findFreeAdjacent gs = do
  playerAxial <- gs ^? game_board . traversed
                     . filtered (has (tile_content . _Just . _Player))
                     . tile_coordinate
  listToMaybe $ filter isFree (neigbours playerAxial)
  where
    isFree axial = hasn't (game_board . ix axial . tile_content . _Just) gs

spawnFriend :: MonadState GameState m => m ()
spawnFriend = do
  gs <- SC.get
  for_ (findFreeAdjacent gs) $ \axial ->
    game_board . ix axial . tile_content ?= Player defUnit

applyShopUpdates :: MonadState GameState m => ShopAction -> m ()
applyShopUpdates = \case
    MkExited -> assign game_shop Nothing
    MkBought bought -> do
      let spawnableItems = Set.filter (\i -> si_type i == ShopUnit) (haulItems bought)
          carryableItems = Set.filter (\i -> si_type i /= ShopUnit) (haulItems bought)
      game_player_inventory . inventroy_item <>= carryableItems
      assign (game_player_inventory . inventory_money) $ haulNewMoney bought
      assign game_shop Nothing
      traverse_ (const spawnFriend) (Set.toList spawnableItems)

-- | Remove any Player unit whose HP has reached zero, leaving a blood splash
--   in their place.  Runs before checkPlayerLives so the lose check is simply
--   "no Player tiles remain".
removeDeadFriends :: MonadState GameState m => m ()
removeDeadFriends = do
  gs <- SC.get
  let isDeadPlayer t = case t ^? tile_content . _Just . _Player of
        Just u  -> isDead (u ^. unit_hp)
        Nothing -> False
      deadAxials = gs ^.. game_board . traversed
                       . filtered isDeadPlayer
                       . tile_coordinate
  for_ deadAxials $ \axial -> do
    game_board . ix axial . tile_content  .= Nothing
    game_board . ix axial . tile_background ?= Blood

checkPlayerLives :: MonadState GameState m => m ()
checkPlayerLives = do
  gs <- SC.get
  when (gs ^. game_phase == Playing) $
    unless (has (game_board . traversed . tile_content . _Just . _Player) gs) $
      game_phase .= YouDied

checkWon :: MonadState GameState m => m ()
checkWon = do
  gs <- SC.get
  when (gs ^. game_phase == Playing) $
    when (hasn't allEnemies gs) $
      game_phase .= YouVictorious

updateState :: GameState -> (RandTNT (), UpdateEvts) -> GameState
updateState gameState (resolveRng, evts) =
  execState (unRandNt resolveRng $ do
                updateLogic evts
                removeDeadFriends
                checkPlayerLives
                checkWon
            ) gameState
