{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Plunder.Level
  ( Level(..)
  , TilePlacement(..)
  , TileContentDef(..)
  , level_grid_begin
  , level_grid_end
  , level_money
  , level_tiles
  , tp_q
  , tp_r
  , tp_content
  , tp_background
  , decodeLevel
  , decodeLevelFile
  , levelToToml
  , defaultLevel
  , tileContentDefToContent
  , tileContentToDef
  ) where

import Control.Lens hiding (Level)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Plunder.Combat
import Plunder.Grid
import Plunder.Shop
import TOML (TOMLError)
import TOML.Decode (Decoder, DecodeTOML(..), getField, getFieldOpt, getFieldOr,
                     getFieldWith, getFieldOptWith, decodeWith)

-- | What kind of content a tile has in a level definition.
--   Unlike 'TileContent', this is a pure data description without runtime state.
data TileContentDef
  = PlayerDef { _tcd_hp :: Health, _tcd_weapon :: Maybe Weapon }
  | EnemyDef  { _tcd_hp :: Health, _tcd_weapon :: Maybe Weapon }
  | HouseDef  { _tcd_hp :: Health }
  | ShopDef   ShopContent
  deriving (Show, Eq)

-- | A single tile placement in a level.
data TilePlacement = MkTilePlacement
  { _tp_q          :: Int
  , _tp_r          :: Int
  , _tp_content    :: Maybe TileContentDef
  , _tp_background :: Maybe Background
  } deriving (Show, Eq)

-- | A complete level definition.
data Level = MkLevel
  { _level_grid_begin :: Int
  , _level_grid_end   :: Int
  , _level_money      :: Word64
  , _level_tiles      :: [TilePlacement]
  } deriving (Show, Eq)

makeLenses ''TilePlacement
makeLenses ''Level

--------------------------------------------------------------------------------
-- Conversion helpers between TileContentDef and TileContent
--------------------------------------------------------------------------------

tileContentDefToContent :: TileContentDef -> TileContent
tileContentDefToContent (PlayerDef hp w) = Player (MkUnit hp w Nothing)
tileContentDefToContent (EnemyDef hp w)  = Enemy (MkUnit hp w Nothing)
tileContentDefToContent (HouseDef hp)    = House (MkUnit hp Nothing Nothing)
tileContentDefToContent (ShopDef sc)     = Shop sc

tileContentToDef :: TileContent -> TileContentDef
tileContentToDef (Player u) = PlayerDef (u ^. unit_hp) (u ^. unit_weapon)
tileContentToDef (Enemy u)  = EnemyDef (u ^. unit_hp) (u ^. unit_weapon)
tileContentToDef (House u)  = HouseDef (u ^. unit_hp)
tileContentToDef (Shop sc)  = ShopDef sc

--------------------------------------------------------------------------------
-- TOML Decoding
--------------------------------------------------------------------------------

weaponFromText :: Text -> Maybe Weapon
weaponFromText t = case T.toLower t of
  "axe"   -> Just Axe
  "bow"   -> Just Bow
  "sword" -> Just Sword
  _       -> Nothing

backgroundFromText :: Text -> Maybe Background
backgroundFromText t = case T.toLower t of
  "blood"        -> Just Blood
  "burned-house" -> Just BurnedHouse
  "burned-shop"  -> Just BurnedShop
  _              -> Nothing

shopItemTypeFromText :: Text -> Maybe ShopItemType
shopItemTypeFromText t = case T.toLower t of
  "health-potion" -> Just ShopHealthPotion
  "unit"          -> Just ShopUnit
  "axe"           -> Just (ShopWeapon Axe)
  "bow"           -> Just (ShopWeapon Bow)
  "sword"         -> Just (ShopWeapon Sword)
  _               -> Nothing

decodeWeapon :: Decoder Weapon
decodeWeapon = do
  t <- tomlDecoder @Text
  case weaponFromText t of
    Just w  -> pure w
    Nothing -> fail "Expected weapon: axe, bow, or sword"

decodeBackground :: Decoder Background
decodeBackground = do
  t <- tomlDecoder @Text
  case backgroundFromText t of
    Just b  -> pure b
    Nothing -> fail "Expected background: blood, burned-house, or burned-shop"

decodeShopItemType :: Decoder ShopItemType
decodeShopItemType = do
  t <- tomlDecoder @Text
  case shopItemTypeFromText t of
    Just sit -> pure sit
    Nothing  -> fail "Expected shop item type: health-potion, unit, axe, bow, or sword"

decodeShopItem :: Decoder ShopItem
decodeShopItem =
  MkShopItem
    <$> getField "price"
    <*> getFieldWith decodeShopItemType "type"

decodeShopContent :: Decoder ShopContent
decodeShopContent =
  MkShopContent
    <$> getFieldOptWith decodeShopItem "slot1"
    <*> getFieldOptWith decodeShopItem "slot2"
    <*> getFieldOptWith decodeShopItem "slot3"

instance DecodeTOML TilePlacement where
  tomlDecoder = do
    _tp_q <- getField "q"
    _tp_r <- getField "r"
    contentTag <- getFieldOpt @Text "content"
    _tp_content <- case contentTag of
      Nothing -> pure Nothing
      Just tag -> case T.toLower tag of
        "player" -> do
          hp <- getField "hp"
          weapon <- getFieldOptWith decodeWeapon "weapon"
          pure $ Just $ PlayerDef hp weapon
        "enemy" -> do
          hp <- getField "hp"
          weapon <- getFieldOptWith decodeWeapon "weapon"
          pure $ Just $ EnemyDef hp weapon
        "house" -> do
          hp <- getField "hp"
          pure $ Just $ HouseDef hp
        "shop" -> do
          sc <- decodeShopContent
          pure $ Just $ ShopDef sc
        _ -> fail $ "Unknown content type: " <> T.unpack tag
    _tp_background <- getFieldOptWith decodeBackground "background"
    pure MkTilePlacement{..}

decodeLevelDecoder :: Decoder Level
decodeLevelDecoder = do
  _level_grid_begin <- getFieldWith (getField "begin") "grid"
  _level_grid_end   <- getFieldWith (getField "end") "grid"
  _level_money      <- getFieldWith (getField "money") "inventory"
  _level_tiles      <- getFieldOr [] "tiles"
  pure MkLevel{..}

-- | Decode a Level from TOML text.
decodeLevel :: Text -> Either TOMLError Level
decodeLevel = decodeWith decodeLevelDecoder

-- | Decode a Level from a TOML file.
decodeLevelFile :: FilePath -> IO (Either TOMLError Level)
decodeLevelFile path = decodeLevel <$> TIO.readFile path

--------------------------------------------------------------------------------
-- TOML Encoding (manual text generation)
--------------------------------------------------------------------------------

weaponToText :: Weapon -> Text
weaponToText Sword = "sword"
weaponToText Bow   = "bow"
weaponToText Axe   = "axe"

backgroundToText :: Background -> Text
backgroundToText Blood       = "blood"
backgroundToText BurnedHouse = "burned-house"
backgroundToText BurnedShop  = "burned-shop"

shopItemTypeToText :: ShopItemType -> Text
shopItemTypeToText ShopHealthPotion = "health-potion"
shopItemTypeToText ShopUnit         = "unit"
shopItemTypeToText (ShopWeapon w)   = weaponToText w

shopItemToToml :: Text -> ShopItem -> Text
shopItemToToml key (MkShopItem price sitType) =
  key <> " = { price = " <> textShow price <> ", type = \"" <> shopItemTypeToText sitType <> "\" }"

textShow :: Show a => a -> Text
textShow = T.pack . show

tilePlacementToToml :: TilePlacement -> Text
tilePlacementToToml tp = T.unlines $
  [ "[[tiles]]"
  , "q = " <> textShow (_tp_q tp)
  , "r = " <> textShow (_tp_r tp)
  ] <> contentLines <> backgroundLines
  where
    contentLines :: [Text]
    contentLines = case _tp_content tp of
      Nothing -> []
      Just (PlayerDef hp weapon) ->
        ["content = \"player\"", "hp = " <> textShow hp]
        <> maybe [] (\w -> ["weapon = \"" <> weaponToText w <> "\""]) weapon
      Just (EnemyDef hp weapon) ->
        ["content = \"enemy\"", "hp = " <> textShow hp]
        <> maybe [] (\w -> ["weapon = \"" <> weaponToText w <> "\""]) weapon
      Just (HouseDef hp) ->
        ["content = \"house\"", "hp = " <> textShow hp]
      Just (ShopDef (MkShopContent s1 s2 s3)) ->
        ["content = \"shop\""]
        <> maybe [] (\s -> [shopItemToToml "slot1" s]) s1
        <> maybe [] (\s -> [shopItemToToml "slot2" s]) s2
        <> maybe [] (\s -> [shopItemToToml "slot3" s]) s3
    backgroundLines :: [Text]
    backgroundLines = case _tp_background tp of
      Nothing -> []
      Just bg -> ["background = \"" <> backgroundToText bg <> "\""]

-- | Render a Level to TOML text.
levelToToml :: Level -> Text
levelToToml lvl = T.intercalate "\n" $
  [ "[grid]"
  , "begin = " <> textShow (_level_grid_begin lvl)
  , "end = " <> textShow (_level_grid_end lvl)
  , ""
  , "[inventory]"
  , "money = " <> textShow (_level_money lvl)
  , ""
  ] <> map tilePlacementToToml (_level_tiles lvl)

--------------------------------------------------------------------------------
-- Default level (matches the previously hardcoded level in State.hs)
--------------------------------------------------------------------------------

-- | The default level, matching the previously hardcoded level in State.hs.
defaultLevel :: Level
defaultLevel = MkLevel
  { _level_grid_begin = 0
  , _level_grid_end   = 6
  , _level_money      = 0
  , _level_tiles =
      [ MkTilePlacement 2 3 (Just (PlayerDef 10 (Just Axe))) Nothing
      , MkTilePlacement 3 3 (Just (HouseDef 10)) Nothing
      , MkTilePlacement 2 6 (Just (ShopDef (MkShopContent
          (Just (MkShopItem 4 ShopHealthPotion))
          (Just (MkShopItem 8 ShopUnit))
          (Just (MkShopItem 5 (ShopWeapon Sword)))))) Nothing
      , MkTilePlacement 4 5 (Just (EnemyDef 10 (Just Axe))) Nothing
      , MkTilePlacement 4 4 (Just (EnemyDef 10 (Just Bow))) Nothing
      , MkTilePlacement 4 3 (Just (EnemyDef 10 (Just Sword))) Nothing
      , MkTilePlacement 0 6 (Just (EnemyDef 10 Nothing)) Nothing
      , MkTilePlacement 1 6 Nothing (Just Blood)
      ]
  }
