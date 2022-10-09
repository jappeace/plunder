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
            , inventory_money
            , inventroy_item
            , PlayerInventory(..)
            , Move(..)
            , Action(..)
            , describeState
            , move_from
            , move_to
            , move
            , RandTNT(..)
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

-- | inidicates the stuff in "pockets", so this doesn't mean equiped
--   equiped is handled by tile content.
data PlayerInventory = MkInventory {
   -- | indicating how much money a player has
    _inventory_money :: Word64
  , _inventroy_item  :: Maybe ShopItem
  } deriving (Show)

data GameState = MkGameState
  { _game_selected :: Maybe Axial
  , _game_board    :: Grid
  , _game_player_inventory :: PlayerInventory
  , _game_shop     :: Maybe ShopContent -- If just we're at the shopping screen
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
  , at (MkAxial 2 6) . _Just . tile_content ?~ Shop (MkShopContent (Just (MkShopItem 4 ShopHealthPotion)) Nothing Nothing)
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
  , _inventroy_item  = Nothing
  }

initialState :: GameState
initialState = MkGameState
  { _game_selected = Nothing
  , _game_board    = appEndo level initialGrid
   -- | indicating how much havoc a player caused,
   --   potentially we could use this later as currency?
  , _game_player_inventory = initialInventory
  , _game_shop = Just (MkShopContent (Just (MkShopItem 4 ShopHealthPotion)) (Just (MkShopItem 4 (ShopWeapon Bow))) (Just (MkShopItem 4 ShopUnit)))
  }

data Attack = MkAttackMove
  { _attack_move :: Move
  , _attack_to   :: TileContent
  } deriving (Show, Eq)

data ShopAction = ShopOpen ShopContent
                | ShopClose
                | ShopBuy ShopItem
              deriving (Show, Eq)

data Action = MkWalk Move -- ^ just go there (no additional events)
              | MkAttack Attack -- ^ play out combat resolution
              | MkShop ShopAction -- ^ Open shop screen
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
  MkShop . ShopOpen <$> preview (traverseBoard towards . _Shop) currentState

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
      MkShop _ -> Nothing

data UpdateEvts = LeftClick Axial
                | RightClick Axial
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
  MkShop _ -> pure Nothing

newtype RandTNT a = MkRandTNT {
  unRandNt :: forall n . Functor n => RandT StdGen n a -> n a
  }

countLoot :: MonadState GameState m => Action -> Result -> m ()
countLoot plan res =
  when (isTargetDead res) $
   when (has (_MkAttack . attack_to . _House) plan) $
         game_player_inventory . inventory_money += 10

updateLogic :: MonadRandom m => MonadState GameState m => UpdateEvts -> m ()
updateLogic = \case
  LeftClick axial -> assign game_selected (Just axial)
  RightClick towards -> do
    currentState <- use id
    let movePlan = shouldCharacterMove currentState towards
                    <|> isShopping currentState towards
                    <|> shouldCharacterAttack currentState towards
    for_ movePlan $ \plan -> trace (show plan) $ do
      mCombatRes <- applyAttack plan
      traverse_ (countLoot plan) mCombatRes
      modifying game_board (figureOutMove mCombatRes plan)
      traverse_ applyShop (plan ^? _MkShop)

applyShop :: MonadState GameState m => ShopAction -> m ()
applyShop = \case
  ShopOpen content -> assign game_shop (Just content)
  ShopClose        -> assign game_shop Nothing
  ShopBuy item     -> assign
                        (game_player_inventory . inventroy_item)
                        (Just item)

resetState :: MonadState GameState m => m ()
resetState = trace "player died, resetting" $ put initialState

checkPlayerLives :: MonadState GameState m => m ()
checkPlayerLives = do
  health <- preuse (game_board . folded . tile_content . _Just . _Player . unit_hp)
  when (maybe True isDead health) resetState

checkWon :: MonadState GameState m => m ()
checkWon = do
  isKill <- hasn't allEnemies <$> SC.get
  when isKill resetState

updateState :: GameState -> (RandTNT (), UpdateEvts) -> GameState
updateState gameState (resolveRng, evts) =
  execState (unRandNt resolveRng $ do
                updateLogic evts
                checkPlayerLives
                checkWon
            ) gameState
