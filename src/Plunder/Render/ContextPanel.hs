{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}

module Plunder.Render.ContextPanel(
  renderContextPanel
  , panelHeight
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.Text (Text)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Set.Lens
import           Data.Word (Word8, Word64)
import           Foreign.C.Types (CInt)
import           Plunder.Combat
import           Plunder.Grid (Terrain(..))
import           Plunder.Render.Color
import           Plunder.Render.Font
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Plunder.Shop
import           Plunder.State
import           Reflex
import           Reflex.SDL2

-- | Fixed height of the context panel at the bottom of the screen.
panelHeight :: CInt
panelHeight = 120

-- | Style used for context panel labels.
panelStyle :: Style
panelStyle = defaultStyle & styleColorLens .~ V4 220 220 220 255

panelHeaderStyle :: Style
panelHeaderStyle = defaultStyle & styleColorLens .~ V4 255 255 255 255

shopSelectedStyle :: Style
shopSelectedStyle = panelStyle & styleColorLens .~ V4 200 30 30 255

-- | Render a context panel at the bottom of the screen showing info about the
--   selected tile.  Returns shop purchase events when a shop is selected.
renderContextPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t GameState -> Dynamic t (V2 CInt) -> m (Event t ShopAction)
renderContextPanel font gameState winSizeDyn = do
  let contextDyn = selectedTileInfo <$> gameState
      moneyDyn   = view (game_player_inventory . inventory_money) <$> gameState
      hasRoomDyn = isJust . findFreeAdjacent <$> gameState

  renderer <- ask

  -- Draw dark panel background
  commitLayer $ ffor2 contextDyn winSizeDyn $ \ctx (V2 w h) ->
    case ctx of
      ContextNone -> pure ()
      _           -> do
        setDrawColor renderer (V4 30 30 30 220)
        fillRect renderer (Just (Rectangle (P $ V2 0 (h - panelHeight)) (V2 w panelHeight)))

  -- Render content using dynView; switchHold flattens the shop events
  shopEvtEvt <- dynView $ renderPanelContent font winSizeDyn moneyDyn hasRoomDyn <$> contextDyn
  switchHold never shopEvtEvt

-- | Position helper: compute a point in the panel given window size and offsets
panelPos :: V2 CInt -> CInt -> CInt -> Point V2 CInt
panelPos (V2 _ h) xOff lineIdx =
  P $ V2 (10 + xOff) (h - panelHeight + 10 + lineIdx * 18)

-- | Display a single text element on screen by rendering and committing it.
panelText
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Style -> Point V2 CInt -> Text -> m ()
panelText font style pos txt = do
  imgSettings <- renderText font style pos txt
  void $ image (constDyn (Just imgSettings))

renderPanelContent
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t (V2 CInt) -> Dynamic t Word64 -> Dynamic t Bool -> ContextInfo -> m (Event t ShopAction)
renderPanelContent _ _ _ _ ContextNone = pure never
renderPanelContent font winSizeDyn _ _ (ContextEmpty terrain) = do
  void $ dynView $ renderEmptyPanel font terrain <$> winSizeDyn
  pure never
renderPanelContent font winSizeDyn _ _ (ContextFog terrain) = do
  void $ dynView $ renderFogPanel font terrain <$> winSizeDyn
  pure never
renderPanelContent font winSizeDyn _ _ (ContextPlayer terrain unit' inv) = do
  void $ dynView $ renderPlayerPanel font terrain unit' inv <$> winSizeDyn
  pure never
renderPanelContent font winSizeDyn _ _ (ContextEnemy terrain unit') = do
  void $ dynView $ renderEnemyPanel font terrain unit' <$> winSizeDyn
  pure never
renderPanelContent font winSizeDyn _ _ (ContextHouse terrain unit') = do
  void $ dynView $ renderHousePanel font terrain unit' <$> winSizeDyn
  pure never
renderPanelContent font winSizeDyn moneyDyn hasRoomDyn (ContextShop terrain content) =
  renderShopPanel font terrain winSizeDyn moneyDyn hasRoomDyn content

--------------------------------------------------------------------------------
-- Individual panel renderers
--------------------------------------------------------------------------------

terrainLabel :: Terrain -> Text
terrainLabel Land      = "Land"
terrainLabel Water     = "Water"
terrainLabel Mountains = "Mountains"

-- | X offset where content-specific info begins (right of terrain column).
contentXOff :: CInt
contentXOff = 120

renderEmptyPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> V2 CInt -> m ()
renderEmptyPanel font terrain winSize = do
  panelText font panelHeaderStyle (panelPos winSize 0 0) (terrainLabel terrain)
  panelText font panelStyle (panelPos winSize contentXOff 0) "Empty"

renderFogPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> V2 CInt -> m ()
renderFogPanel font terrain winSize = do
  panelText font panelHeaderStyle (panelPos winSize 0 0) (terrainLabel terrain)
  panelText font panelStyle (panelPos winSize contentXOff 0) "Fog of war"

renderPlayerPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> Unit -> PlayerInventory -> V2 CInt -> m ()
renderPlayerPanel font terrain unit' inv winSize = do
  panelText font panelHeaderStyle (panelPos winSize 0 0) (terrainLabel terrain)
  panelText font panelHeaderStyle (panelPos winSize contentXOff 0) "Player"
  panelText font panelStyle (panelPos winSize contentXOff 1)
    ("HP: " <> tshow (unit' ^. unit_hp) <> "/" <> tshow maxHealth)
  panelText font panelStyle (panelPos winSize contentXOff 2)
    ("Weapon: " <> maybe "none" weaponDescription (unit' ^. unit_weapon))
  panelText font panelStyle (panelPos winSize contentXOff 3)
    (describeStatus (unit' ^. unit_status))
  panelText font panelStyle (panelPos winSize 320 0)
    ("Gold: " <> tshow (inv ^. inventory_money))
  panelText font panelStyle (panelPos winSize 320 1)
    ("Items: " <> tshow (length (inv ^. inventroy_item)))

renderEnemyPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> Unit -> V2 CInt -> m ()
renderEnemyPanel font terrain unit' winSize = do
  panelText font panelHeaderStyle (panelPos winSize 0 0) (terrainLabel terrain)
  panelText font panelHeaderStyle (panelPos winSize contentXOff 0) "Enemy"
  panelText font panelStyle (panelPos winSize contentXOff 1)
    ("HP: " <> tshow (unit' ^. unit_hp) <> "/" <> tshow maxHealth)
  panelText font panelStyle (panelPos winSize contentXOff 2)
    ("Weapon: " <> maybe "none" weaponDescription (unit' ^. unit_weapon))
  panelText font panelStyle (panelPos winSize contentXOff 3)
    (describeStatus (unit' ^. unit_status))

renderHousePanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> Unit -> V2 CInt -> m ()
renderHousePanel font terrain unit' winSize = do
  panelText font panelHeaderStyle (panelPos winSize 0 0) (terrainLabel terrain)
  panelText font panelHeaderStyle (panelPos winSize contentXOff 0) "House"
  panelText font panelStyle (panelPos winSize contentXOff 1)
    ("HP: " <> tshow (unit' ^. unit_hp) <> "/" <> tshow maxHealth)

describeStatus :: Maybe StatusEffect -> Text
describeStatus Nothing = "Status: none"
describeStatus (Just DrinkingPotion) = "Status: drinking"
describeStatus (Just (Healing n)) = "Status: healing (" <> tshow n <> ")"

--------------------------------------------------------------------------------
-- Shop panel (interactive)
--------------------------------------------------------------------------------

newtype ShopSelection = MkShopSelection
  { selectedSlots :: Map Word8 (ShopContent -> Maybe ShopItem) }

initialSelection :: ShopSelection
initialSelection = MkShopSelection mempty

toggleSlot :: ShopSelection -> (Word8, ShopContent -> Maybe ShopItem) -> ShopSelection
toggleSlot (MkShopSelection m) (idx, slot) = MkShopSelection $
  if Map.member idx m then Map.delete idx m
  else Map.insert idx slot m

data PurchaseError = ShopClosed
                   | NotEnoughMoney
                   | NoItemsSelected
                   | NoRoomForFriend

renderPurchaseError :: PurchaseError -> Text
renderPurchaseError = \case
  ShopClosed      -> "Shop closed"
  NotEnoughMoney  -> "Insufficient money"
  NoItemsSelected -> "Nothing selected"
  NoRoomForFriend -> "No room for friend"

purchaseAction :: Word64 -> Bool -> ShopSelection -> ShopContent -> Either PurchaseError Haul
purchaseAction playerMoney hasRoom sel content =
  let counter :: Set ShopItem
      counter = setOf (traversed . to (\fun -> fun content) . traversed) $ selectedSlots sel
      price = sumOf (folded . si_priceLens) counter
      wantsFriend = any (\i -> si_type i == ShopUnit) counter
  in if | length counter < 1          -> Left NoItemsSelected
        | playerMoney < price         -> Left NotEnoughMoney
        | wantsFriend && not hasRoom  -> Left NoRoomForFriend
        | True -> Right $ MkHaul
            { haulItems = counter
            , haulNewMoney = playerMoney - price
            }

renderShopPanel
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Terrain -> Dynamic t (V2 CInt) -> Dynamic t Word64 -> Dynamic t Bool -> ShopContent -> m (Event t ShopAction)
renderShopPanel font terrain winSizeDyn moneyDyn hasRoomDyn content = mdo
  -- Terrain (left) and header (right)
  void $ dynView $ (\ws -> do
    panelText font panelHeaderStyle (panelPos ws 0 0) (terrainLabel terrain)
    panelText font panelHeaderStyle (panelPos ws contentXOff 0) "Shop"
    ) <$> winSizeDyn

  -- Shop slots (right of terrain)
  let slots :: [(Word8, ShopContent -> Maybe ShopItem)]
      slots = [(0, slot1), (1, slot2), (2, slot3)]

  slotEvts <- forM slots $ \(idx, slotAccessor) ->
    fmap ((idx, slotAccessor) <$) $
      image =<< holdDyn Nothing =<< dynView (renderShopSlot font content idx slotAccessor <$> winSizeDyn <*> selDyn)

  selDyn <- accumDyn toggleSlot initialSelection $ leftmost slotEvts

  -- Purchase button
  purchaseSurface <- allocateText font panelStyle "Purchase"
  let purchasePosDyn = (\ws -> surfaceToSettings purchaseSurface (panelPos ws 320 0)) <$> winSizeDyn
  purchaseClick <- image (Just <$> purchasePosDyn)

  let purchaseResult = (\money hasRoom sel -> purchaseAction money hasRoom sel content)
        <$> current moneyDyn <*> current hasRoomDyn <*> current selDyn
      (purchaseError, purchaseSuccess) = fanEither $ purchaseResult <@ purchaseClick

  -- Error display
  small <- smallFont
  void $ image =<< holdView (pure Nothing)
    (sequence . renderErrorText small <$> leftmost
      [ Just <$> purchaseError
      , Nothing <$ updated selDyn  -- clear error when selection changes
      ])

  -- Exit button
  exitSurface <- allocateText font panelStyle "Exit"
  let exitPosDyn = (\ws -> surfaceToSettings exitSurface (panelPos ws 320 2)) <$> winSizeDyn
  exitClick <- image (Just <$> exitPosDyn)

  pure $ leftmost [MkBought <$> purchaseSuccess, MkExited <$ exitClick]

renderErrorText
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> Maybe PurchaseError -> Maybe (m ImageSettings)
renderErrorText _ Nothing = Nothing
renderErrorText small (Just err) = Just $ renderText small panelStyle (P $ V2 300 10) (renderPurchaseError err)

renderShopSlot
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> ShopContent -> Word8 -> (ShopContent -> Maybe ShopItem) -> V2 CInt -> ShopSelection -> m (Maybe ImageSettings)
renderShopSlot font content idx slotAccessor winSize sel = do
  let style = if Map.member idx (selectedSlots sel) then shopSelectedStyle else panelStyle
      lineIdx = fromIntegral idx + 1
  case slotAccessor content of
    Nothing   -> Just <$> renderText font panelStyle (panelPos winSize contentXOff lineIdx) "-"
    Just item -> Just <$> renderText font style (panelPos winSize contentXOff lineIdx) (itemDescription item)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
