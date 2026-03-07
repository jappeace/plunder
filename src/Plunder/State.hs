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
            , levelToGameState
            , gameStateToLevel
            , UpdateEvts(..)
            , game_board
            , game_selected
            , game_player_inventory
            , game_shop
            , game_inventory_open
            , game_phase
            , game_planned_moves
            , game_pending_purchase
            , game_explored
            , inventory_money
            , inventroy_item
            , PlayerInventory(..)
            , Move(..)
            , Action(..)
            , GamePhase(..)
            , Visibility(..)
            , describeState
            , move_from
            , move_to
            , move
            , RandTNT(..)
            , findFreeAdjacent
            , playerPositions
            , tileVisibility
            ) where

import qualified Control.Monad.State.Class as SC
import           Plunder.Combat
import           Plunder.Level
import           Control.Lens hiding (Level)
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
import           Plunder.Pathfinding
import           System.Random
import           Text.Printf
import Plunder.Shop
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe(listToMaybe, mapMaybe, catMaybes)

data GamePhase = Playing | YouDied | YouVictorious deriving (Show, Eq)

data Visibility = Visible | Fog | Unexplored deriving (Show, Eq)

-- | inidicates the stuff in "pockets", so this doesn't mean equiped
--   equiped is handled by tile content.
data PlayerInventory = MkInventory {
   -- | indicating how much money a player has
    _inventory_money :: Word64
  , _inventroy_item  :: Set ShopItem
  } deriving (Show)

data GameState = MkGameState
  { _game_selected          :: Maybe Axial
  , _game_board             :: Grid
  , _game_player_inventory  :: PlayerInventory
  , _game_shop              :: Maybe ShopContent -- If just we're at the shopping screen
  , _game_inventory_open    :: Bool
  , _game_phase             :: GamePhase
  , _game_planned_moves     :: Map Axial [Axial]  -- ^ from -> path (excluding src): queued player moves
  , _game_pending_purchase  :: Maybe Haul        -- ^ purchase queued, applied on EndTurn
  , _game_explored          :: Set Axial         -- ^ tiles that have been within visibility range
  } deriving (Show)
makeLenses ''GameState
makeLenses ''PlayerInventory

-- filters out irrelevant stuff
describeState :: GameState -> String
describeState x = printf "describeState { _game_selected = %s, _game_shop = %s }"
  (show (x ^. game_selected))
  (show (x ^. game_shop))
  -- (show (x ^.. game_board . contentFold))

--------------------------------------------------------------------------------
-- Fog of war
--------------------------------------------------------------------------------

-- | Get all player positions from the board.
playerPositions :: GameState -> [Axial]
playerPositions gs = gs ^.. game_board . traversed
                         . filtered (has (tile_content . _Just . _Player))
                         . tile_coordinate

-- | Compute visibility for a single tile based on distance to nearest player.
tileVisibility :: GameState -> Axial -> Visibility
tileVisibility gs axial =
  case playerPositions gs of
    [] -> Unexplored
    ps ->
      let minDist = minimum $ fmap (hexDistance axial) ps
      in if minDist < 3 then Visible
         else if minDist <= 4 then Fog
         else if Set.member axial (gs ^. game_explored) then Fog
         else Unexplored

-- | Compute the set of tiles currently within sight range (distance < 5).
computeNewlyExplored :: GameState -> Set Axial
computeNewlyExplored gs =
  Set.fromList
    [ axial
    | axial <- Map.keys (gs ^. game_board)
    , p     <- playerPositions gs
    , hexDistance axial p < 5
    ]

-- | Union the currently visible tiles into the explored set.
updateExplored :: MonadState GameState m => m ()
updateExplored = do
  gs <- use id
  game_explored <>= computeNewlyExplored gs

--------------------------------------------------------------------------------
-- Level conversion
--------------------------------------------------------------------------------

-- | Convert a Level to a GameState.
levelToGameState :: Level -> GameState
levelToGameState lvl =
  let gs0 = MkGameState
        { _game_selected         = Nothing
        , _game_board            = applyPlacements (lvl ^. level_tiles) baseGrid
        , _game_player_inventory = MkInventory (lvl ^. level_money) Set.empty
        , _game_shop             = Nothing
        , _game_inventory_open   = False
        , _game_phase            = Playing
        , _game_planned_moves    = Map.empty
        , _game_pending_purchase = Nothing
        , _game_explored         = Set.empty
        }
  in gs0 & game_explored .~ computeNewlyExplored gs0
  where
    baseGrid :: Grid
    baseGrid = mkGrid (lvl ^. level_grid_begin) (lvl ^. level_grid_end)

    applyPlacements :: [TilePlacement] -> Grid -> Grid
    applyPlacements tps g = appEndo (foldMap (Endo . applyPlacement) tps) g

    applyPlacement :: TilePlacement -> Grid -> Grid
    applyPlacement tp g = g
      & maybe id (\c -> at axial . _Just . tile_content ?~ tileContentDefToContent c) (tp ^. tp_content)
      & maybe id (\b -> at axial . _Just . tile_background ?~ b) (tp ^. tp_background)
      & maybe id (\t -> at axial . _Just . tile_terrain .~ t) (tp ^. tp_terrain)
      where
        axial :: Axial
        axial = MkAxial (tp ^. tp_q) (tp ^. tp_r)

-- | Extract a Level from a GameState.
gameStateToLevel :: Int -> Int -> GameState -> Level
gameStateToLevel begin end gs = MkLevel
  { _level_grid_begin = begin
  , _level_grid_end   = end
  , _level_money      = gs ^. game_player_inventory . inventory_money
  , _level_tiles      = mapMaybe tileToPlacement (Map.elems (gs ^. game_board))
  }
  where
    tileToPlacement :: Tile -> Maybe TilePlacement
    tileToPlacement tile
      | hasn't (tile_content . _Just) tile
        && hasn't (tile_background . _Just) tile
        && has (tile_terrain . _Land) tile
        = Nothing
      | otherwise = Just $ MkTilePlacement
          { _tp_q          = tile ^. tile_coordinate . _q
          , _tp_r          = tile ^. tile_coordinate . _r
          , _tp_content    = tileContentToDef <$> tile ^. tile_content
          , _tp_background = tile ^. tile_background
          , _tp_terrain    = if has (tile_terrain . _Land) tile then Nothing else Just (tile ^. tile_terrain)
          }

--------------------------------------------------------------------------------
-- Initial state
--------------------------------------------------------------------------------

allEnemies :: Traversal' GameState Unit
allEnemies = game_board . traversed . tile_content . _Just . _Enemy

initialState :: GameState
initialState = levelToGameState defaultLevel

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
  && has (game_board . ix towards . tile_terrain . _Land) state'

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
                | UseItem ShopItem
                deriving (Show, Eq)

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

-- | If a Player is selected, compute a BFS path to @towards@ (or return an
--   empty path when @towards == selectedAxial@ to signal cancellation).
--   Adjacent shops are handled immediately by the RightClick handler before
--   this function is reached; distant shops are pathable so the player walks
--   toward them (the final step onto the shop is skipped by executePlannedMove).
planPlayerMove :: GameState -> Axial -> Maybe (Axial, [Axial])
planPlayerMove state' towards = do
  selectedAxial <- state' ^. game_selected
  _player :: Unit <- state' ^? game_board . ix selectedAxial . tile_content . _Just . _Player
  if towards == selectedAxial
    then pure (selectedAxial, [])
    else do
      path <- findPath (state' ^. game_board) selectedAxial towards
      guard (not (null path))
      pure (selectedAxial, path)

-- | Re-derive a walk or attack action at execution time so that plans that
--   became invalid are silently skipped.
executePlannedMove :: GameState -> Axial -> Axial -> Maybe Action
executePlannedMove gs src dst = do
  unit' <- gs ^? game_board . ix src . tile_content . _Just . _Player
  let baseMove = MkMove { _move_from = src, _move_to = dst, _move_from_unit = unit' }
  case isAttack gs dst of
    Just (Shop _) -> Nothing   -- shops are never queued
    Just target   -> Just $ MkAttack $ MkAttackMove
                       { _attack_move = baseMove, _attack_to = target }
    Nothing       -> if isMove gs dst then Just (MkWalk baseMove) else Nothing

-- | The first argument is the state to reset to on 'ResetGame'.
updateLogic :: MonadRandom m => MonadState GameState m => GameState -> UpdateEvts -> m ()
updateLogic resetTo = \case
  Redraw -> pure ()
  ToggleInventory -> modifying game_inventory_open not
  ShopUpdates actions -> applyShopUpdates actions
  LeftClick axial -> assign game_selected (Just axial)
  ResetGame -> do
    put resetTo
    updateExplored
  UseItem item -> applyUseItem item
  EndTurn -> do
    applyPendingPurchase
    applyStatusEffects
    plans <- use game_planned_moves
    game_planned_moves .= Map.empty
    survivors <- fmap (Map.fromList . catMaybes) $ forM (Map.toList plans) $ \(src, path) ->
      case path of
        []         -> pure Nothing   -- empty path, nothing to do
        (dst:rest) -> do
          gs <- use id
          case executePlannedMove gs src dst of
            Nothing   -> pure Nothing   -- invalid step, discard entire path
            Just plan -> do
              mCombatRes <- applyAttack plan
              traverse_ (countLoot plan) mCombatRes
              modifying game_board (figureOutMove mCombatRes plan)
              case plan of
                MkWalk _  | not (null rest) -> pure (Just (dst, rest))
                _                           -> pure Nothing
    game_planned_moves .= survivors
    game_selected .= Nothing
    updateExplored
  RightClick towards -> do
    currentState <- use id
    -- Only shopping is immediate; walks and attacks are queued.
    let immediateAction = isShopping currentState towards
    case immediateAction of
      Just plan -> trace (show plan) $ do
        mCombatRes <- applyAttack plan
        traverse_ (countLoot plan) mCombatRes
        modifying game_board (figureOutMove mCombatRes plan)
        traverse_ applyShop (plan ^? _OpenShop)
      Nothing ->
        for_ (planPlayerMove currentState towards) $ \(src, path) ->
          if null path
            then game_planned_moves . at src .= Nothing       -- cancel move, keep purchase
            else do
              game_planned_moves . at src ?= path             -- plan or overwrite
              game_pending_purchase .= Nothing                -- moving cancels purchase

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
      assign game_shop Nothing
      game_pending_purchase .= Just bought

-- | Apply and clear any pending purchase.  Called at the start of EndTurn.
applyPendingPurchase :: MonadState GameState m => m ()
applyPendingPurchase = do
  mHaul <- use game_pending_purchase
  game_pending_purchase .= Nothing
  for_ mHaul $ \haul -> do
    let spawnableItems = Set.filter (\i -> si_type i == ShopUnit) (haulItems haul)
        carryableItems = Set.filter (\i -> si_type i /= ShopUnit) (haulItems haul)
    game_player_inventory . inventroy_item <>= carryableItems
    assign (game_player_inventory . inventory_money) $ haulNewMoney haul
    traverse_ (const spawnFriend) (Set.toList spawnableItems)

-- | Resolve which player unit to apply an inventory item to.
--   Prefers the currently selected Player; falls back to the first Player on
--   the board so items work without requiring an explicit re-selection.
itemTarget :: GameState -> Maybe Axial
itemTarget gs = case gs ^. game_selected of
  Just axial | has (game_board . ix axial . tile_content . _Just . _Player) gs -> Just axial
  _          -> gs ^? game_board . traversed
                        . filtered (has (tile_content . _Just . _Player))
                        . tile_coordinate

applyUseItem :: MonadState GameState m => ShopItem -> m ()
applyUseItem item = do
  gs <- use id
  for_ (itemTarget gs) $ \axial -> do
    game_player_inventory . inventroy_item %= Set.delete item
    case si_type item of
      ShopHealthPotion ->
        game_board . ix axial . tile_content . _Just . _Player . unit_status ?= DrinkingPotion
      ShopWeapon newWeapon -> do
        mOldWeapon <- preuse (game_board . ix axial . tile_content . _Just . _Player . unit_weapon . _Just)
        game_board . ix axial . tile_content . _Just . _Player . unit_weapon .= Just newWeapon
        for_ mOldWeapon $ \oldWeapon ->
          game_player_inventory . inventroy_item %= Set.insert (MkShopItem 0 (ShopWeapon oldWeapon))
      ShopUnit -> pure ()

healAmount :: Health
healAmount = maxHealth `div` 10

heal :: Unit -> Unit
heal unit = unit & unit_hp %~ min maxHealth . (+ healAmount)

tickStatus :: Unit -> Unit
tickStatus unit = case unit ^. unit_status of
  Nothing             -> unit
  Just DrinkingPotion -> heal unit & unit_status .~ Just (Healing 4)
  Just (Healing 0)    -> unit & unit_status .~ Nothing
  Just (Healing n)    -> heal unit & unit_status .~ Just (Healing (n - 1))

applyStatusEffects :: MonadState GameState m => m ()
applyStatusEffects =
  game_board . traversed . tile_content . _Just . _Player %= tickStatus

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

-- | The first argument is the state to reset to on 'ResetGame'.
updateState :: GameState -> GameState -> (RandTNT (), UpdateEvts) -> GameState
updateState resetTo gameState (resolveRng, evts) =
  execState (unRandNt resolveRng $ do
                updateLogic resetTo evts
                removeDeadFriends
                checkPlayerLives
                checkWon
            ) gameState
