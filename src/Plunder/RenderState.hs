{-# LANGUAGE TemplateHaskell #-}

-- | Pure intermediate representation between 'GameState' and rendering.
--   Describes the visual scene at an abstract level without SDL types.
module Plunder.RenderState
  ( -- * Sprite enums
    TileSprite(..)
  , BackgroundSprite(..)
  , WeaponSprite(..)
    -- * Per-tile data
  , HealthBarInfo(..)
  , hb_currentHp
  , hb_maxHp
  , RenderTile(..)
  , rtile_coordinate
  , rtile_terrain
  , rtile_sprite
  , rtile_weapon
  , rtile_background
  , rtile_healthBar
  , rtile_visibility
    -- * Planned arrows
  , PlannedArrow(..)
  , arrow_source
  , arrow_waypoints
    -- * HUD types
  , MoneyDisplay(..)
  , money_amount
  , PurchaseLabel(..)
  , plabel_text
  , plabel_playerPos
  , InventoryInfo(..)
  , invInfo_isOpen
  , invInfo_items
  , BannerState(..)
  , HelpState(..)
  , HudState(..)
  , hud_money
  , hud_purchaseLabel
  , hud_contextPanel
  , hud_hasRoom
  , hud_inventory
  , hud_banner
  , hud_help
    -- * Top-level render state
  , RenderState(..)
  , render_tiles
  , render_selectedTile
  , render_camera
  , render_plannedArrows
  , render_hud
    -- * Conversion
  , gameStateToRenderState
    -- * Re-exports used by renderers
  , Visibility(..)
    -- * Helpers (exported for testing)
  , tileContentToSprite
  , tileContentToWeapon
  , backgroundToSprite
  ) where

import           Control.Lens hiding (Level)
import           Data.Monoid (Any)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8, Word64)
import           Foreign.C.Types (CInt)
import           SDL.Vect (V2)

import           Plunder.Combat (Health, Unit, Weapon(..), maxHealth, unit_hp, unit_weapon)
import           Plunder.Grid
  ( Axial(..), Tile, TileContent(..), Background(..), Terrain(..)
  , tile_coordinate, tile_content, tile_background, tile_terrain
  , tc_unit, _Player
  )
import           Plunder.Shop
  ( ShopItem, Haul(..)
  , itemTypeDescription, si_type
  )
import           Plunder.State
  ( GameState(..), GamePhase(..), Visibility(..)
  , ContextInfo(..)
  , game_board, game_selected, game_player_inventory, game_phase
  , game_planned_moves, game_pending_purchase, game_camera
  , game_inventory_open
  , inventory_money, inventroy_item
  , tileVisibility, selectedTileInfo, findFreeAdjacent
  )

-- | Which sprite to draw on a tile.
data TileSprite
  = PlayerSprite
  | EnemySprite
  | HouseSprite
  | ShopSprite
  | BoatSprite
  deriving (Show, Eq)

-- | Decorative background sprite.
data BackgroundSprite
  = BloodSplash
  | BurnedHouseBg
  | BurnedShopBg
  deriving (Show, Eq)

-- | Weapon overlay sprite.
data WeaponSprite
  = SwordSprite
  | BowSprite
  | AxeSprite
  deriving (Show, Eq)

-- | Health bar data for rendering.
data HealthBarInfo = MkHealthBarInfo
  { _hb_currentHp :: Health
  , _hb_maxHp     :: Health
  } deriving (Show, Eq)

-- | Pure description of a tile for rendering.
data RenderTile = MkRenderTile
  { _rtile_coordinate :: Axial
  , _rtile_terrain    :: Terrain
  , _rtile_sprite     :: Maybe TileSprite
  , _rtile_weapon     :: Maybe WeaponSprite
  , _rtile_background :: Maybe BackgroundSprite
  , _rtile_healthBar  :: Maybe HealthBarInfo
  , _rtile_visibility :: Visibility
  } deriving (Show, Eq)

-- | A planned move shown as arrows on the map.
data PlannedArrow = MkPlannedArrow
  { _arrow_source    :: Axial
  , _arrow_waypoints :: [Axial]
  } deriving (Show, Eq)

-- | Money display in the HUD.
data MoneyDisplay = MkMoneyDisplay
  { _money_amount :: Word64
  } deriving (Show, Eq)

-- | Label shown above the player tile when a purchase is queued.
data PurchaseLabel = MkPurchaseLabel
  { _plabel_text      :: Text
  , _plabel_playerPos :: Axial
  } deriving (Show, Eq)

-- | Inventory panel state.
data InventoryInfo = MkInventoryInfo
  { _invInfo_isOpen :: Bool
  , _invInfo_items  :: Set ShopItem
  } deriving (Show, Eq)

-- | Banner overlay state.
data BannerState
  = NoBanner
  | DeathBanner Word8
  | VictoryBanner Word8
  deriving (Show, Eq)

-- | Help overlay state.
data HelpState
  = HelpHidden
  | HelpVisible
  deriving (Show, Eq)

-- | Heads-up display state.
data HudState = MkHudState
  { _hud_money         :: MoneyDisplay
  , _hud_purchaseLabel :: Maybe PurchaseLabel
  , _hud_contextPanel  :: ContextInfo
  , _hud_hasRoom       :: Bool
  , _hud_inventory     :: InventoryInfo
  , _hud_banner        :: BannerState
  , _hud_help          :: HelpState
  } deriving (Show, Eq)

-- | Complete render state: everything needed to draw one frame.
data RenderState = MkRenderState
  { _render_tiles         :: Map Axial RenderTile
  , _render_selectedTile  :: Maybe Axial
  , _render_camera        :: V2 CInt
  , _render_plannedArrows :: [PlannedArrow]
  , _render_hud           :: HudState
  } deriving (Show, Eq)

makeLenses ''HealthBarInfo
makeLenses ''RenderTile
makeLenses ''PlannedArrow
makeLenses ''MoneyDisplay
makeLenses ''PurchaseLabel
makeLenses ''InventoryInfo
makeLenses ''HudState
makeLenses ''RenderState

--------------------------------------------------------------------------------
-- Conversion helpers
--------------------------------------------------------------------------------

-- | Map tile content to its sprite enum.
tileContentToSprite :: TileContent -> TileSprite
tileContentToSprite (Player _) = PlayerSprite
tileContentToSprite (Enemy _)  = EnemySprite
tileContentToSprite (House _)  = HouseSprite
tileContentToSprite (Shop _)   = ShopSprite
tileContentToSprite Boat       = BoatSprite

-- | Extract the weapon sprite from tile content, if any.
tileContentToWeapon :: TileContent -> Maybe WeaponSprite
tileContentToWeapon content = case content ^? tc_unit . unit_weapon . _Just of
  Just Sword -> Just SwordSprite
  Just Bow   -> Just BowSprite
  Just Axe   -> Just AxeSprite
  Nothing    -> Nothing

-- | Map background decoration to its sprite enum.
backgroundToSprite :: Background -> BackgroundSprite
backgroundToSprite Blood       = BloodSplash
backgroundToSprite BurnedHouse = BurnedHouseBg
backgroundToSprite BurnedShop  = BurnedShopBg

-- | Build a health bar for a tile's unit content (only if the unit is alive).
buildHealthBar :: TileContent -> Maybe HealthBarInfo
buildHealthBar content = do
  hp <- content ^? tc_unit . unit_hp
  if hp <= 0
    then Nothing
    else Just $ MkHealthBarInfo hp maxHealth

-- | Build a single 'RenderTile' from a 'Tile' and its visibility.
buildRenderTile :: Visibility -> Tile -> RenderTile
buildRenderTile vis tile = MkRenderTile
  { _rtile_coordinate = tile ^. tile_coordinate
  , _rtile_terrain    = tile ^. tile_terrain
  , _rtile_sprite     = tileContentToSprite <$> tile ^. tile_content
  , _rtile_weapon     = tile ^. tile_content >>= tileContentToWeapon
  , _rtile_background = backgroundToSprite <$> tile ^. tile_background
  , _rtile_healthBar  = tile ^. tile_content >>= buildHealthBar
  , _rtile_visibility = vis
  }

-- | Axial range rendered as terrain (covers the visible screen and one-hex
--   border on each side so that "outside the grid" shows as water).
terrainCoords :: [Axial]
terrainCoords = [MkAxial q r | q <- [-1 .. 7], r <- [-1 .. 7]]

-- | Build the full tile map for rendering, including border coordinates
--   outside the grid that are rendered as Water for the ocean boundary.
buildRenderTiles :: GameState -> Map Axial RenderTile
buildRenderTiles gs =
  let board = gs ^. game_board
      gridTiles = Map.mapWithKey
        (\axial tile -> buildRenderTile (tileVisibility gs axial) tile)
        board
      borderTiles = Map.fromList
        [ (axial, MkRenderTile
            { _rtile_coordinate = axial
            , _rtile_terrain    = Water
            , _rtile_sprite     = Nothing
            , _rtile_weapon     = Nothing
            , _rtile_background = Nothing
            , _rtile_healthBar  = Nothing
            , _rtile_visibility = tileVisibility gs axial
            })
        | axial <- terrainCoords
        , not (Map.member axial board)
        ]
  in Map.union gridTiles borderTiles

-- | Build planned arrows from the game state.
buildPlannedArrows :: GameState -> [PlannedArrow]
buildPlannedArrows gs =
  [ MkPlannedArrow src path
  | (src, path) <- Map.toList (gs ^. game_planned_moves)
  ]

-- | Describe a pending purchase as text.
describePurchase :: Haul -> Text
describePurchase haul =
  "Purchasing " <> T.intercalate ", " (itemTypeDescription . si_type <$> toList (haulItems haul))
  where
    toList = foldr (:) []

-- | Build the purchase label if there's a pending purchase and a player tile.
buildPurchaseLabel :: GameState -> Maybe PurchaseLabel
buildPurchaseLabel gs = do
  haul <- gs ^. game_pending_purchase
  playerPos <- gs ^? game_board . traversed
                    . filtered (has (tile_content . _Just . _Player :: Getting Any Tile Unit))
                    . tile_coordinate
  pure $ MkPurchaseLabel (describePurchase haul) playerPos

-- | Build the HUD state.
buildHud :: Word8 -> Bool -> GameState -> HudState
buildHud bannerAlpha helpOpen gs = MkHudState
  { _hud_money = MkMoneyDisplay (gs ^. game_player_inventory . inventory_money)
  , _hud_purchaseLabel = buildPurchaseLabel gs
  , _hud_contextPanel = selectedTileInfo gs
  , _hud_hasRoom = hasRoom
  , _hud_inventory = MkInventoryInfo
      (gs ^. game_inventory_open)
      (gs ^. game_player_inventory . inventroy_item)
  , _hud_banner = case gs ^. game_phase of
      Playing       -> NoBanner
      YouDied       -> DeathBanner bannerAlpha
      YouVictorious -> VictoryBanner bannerAlpha
  , _hud_help = if helpOpen then HelpVisible else HelpHidden
  }
  where
    hasRoom :: Bool
    hasRoom = case findFreeAdjacent gs of
      Just _  -> True
      Nothing -> False

-- | Convert a 'GameState' to a pure 'RenderState'.
--   Takes the banner alpha and help-open flag as extra parameters since
--   those are held in Reflex Dynamics, not in 'GameState'.
gameStateToRenderState :: Word8 -> Bool -> GameState -> RenderState
gameStateToRenderState bannerAlpha helpOpen gs = MkRenderState
  { _render_tiles         = buildRenderTiles gs
  , _render_selectedTile  = gs ^. game_selected
  , _render_camera        = gs ^. game_camera
  , _render_plannedArrows = buildPlannedArrows gs
  , _render_hud           = buildHud bannerAlpha helpOpen gs
  }
