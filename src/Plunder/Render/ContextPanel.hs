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
      ContextEmpty _ -> pure ()
      _              -> do
        setDrawColor renderer (V4 30 30 30 220)
        fillRect renderer (Just (Rectangle (P $ V2 0 (h - panelHeight)) (V2 w panelHeight)))

  -- Render content using dynView; switchHold flattens the shop events
  shopEvtEvt <- dynView $ renderPanelContent font winSizeDyn moneyDyn hasRoomDyn <$> contextDyn
  switchHold never shopEvtEvt

-- | Position helper: compute a point in the panel given window size and offsets
panelPos :: V2 CInt -> CInt -> CInt -> Point V2 CInt
panelPos (V2 _ h) xOff lineIdx =
  P $ V2 (10 + xOff) (h - panelHeight + 10 + lineIdx * 18)

renderPanelContent
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader Renderer m
  => Font -> Dynamic t (V2 CInt) -> Dynamic t Word64 -> Dynamic t Bool -> ContextInfo -> m (Event t ShopAction)
renderPanelContent _ _ _ _ (ContextEmpty _) = pure never
renderPanelContent font winSizeDyn _ _ ContextFog = do
  void $ image =<< holdDyn Nothing =<< dynView (renderFogPanel font <$> winSizeDyn)
  pure never
renderPanelContent font winSizeDyn _ _ (ContextPlayer unit' inv) = do
  void $ image =<< holdDyn Nothing =<< dynView (renderPlayerPanel font unit' inv <$> winSizeDyn)
  pure never
renderPanelContent font winSizeDyn _ _ (ContextEnemy unit') = do
  void $ image =<< holdDyn Nothing =<< dynView (renderEnemyPanel font unit' <$> winSizeDyn)
  pure never
renderPanelContent font winSizeDyn _ _ (ContextHouse unit') = do
  void $ image =<< holdDyn Nothing =<< dynView (renderHousePanel font unit' <$> winSizeDyn)
  pure never
renderPanelContent font winSizeDyn moneyDyn hasRoomDyn (ContextShop content) =
  renderShopPanel font winSizeDyn moneyDyn hasRoomDyn content

--------------------------------------------------------------------------------
-- Individual panel renderers
--------------------------------------------------------------------------------

renderFogPanel
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> V2 CInt -> m (Maybe ImageSettings)
renderFogPanel font winSize =
  Just <$> renderText font panelHeaderStyle (panelPos winSize 0 0) "Fog of war"

renderPlayerPanel
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> Unit -> PlayerInventory -> V2 CInt -> m (Maybe ImageSettings)
renderPlayerPanel font unit' inv winSize = do
  void $ renderText font panelHeaderStyle (panelPos winSize 0 0) "Player"
  void $ renderText font panelStyle (panelPos winSize 0 1)
    ("HP: " <> tshow (unit' ^. unit_hp) <> "/" <> tshow maxHealth)
  void $ renderText font panelStyle (panelPos winSize 0 2)
    ("Weapon: " <> maybe "none" weaponDescription (unit' ^. unit_weapon))
  void $ renderText font panelStyle (panelPos winSize 0 3)
    (describeStatus (unit' ^. unit_status))
  void $ renderText font panelStyle (panelPos winSize 150 0)
    ("Gold: " <> tshow (inv ^. inventory_money))
  Just <$> renderText font panelStyle (panelPos winSize 150 1)
    ("Items: " <> tshow (length (inv ^. inventroy_item)))

renderEnemyPanel
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> Unit -> V2 CInt -> m (Maybe ImageSettings)
renderEnemyPanel font unit' winSize = do
  void $ renderText font panelHeaderStyle (panelPos winSize 0 0) "Enemy"
  void $ renderText font panelStyle (panelPos winSize 0 1)
    ("HP: " <> tshow (unit' ^. unit_hp) <> "/" <> tshow maxHealth)
  void $ renderText font panelStyle (panelPos winSize 0 2)
    ("Weapon: " <> maybe "none" weaponDescription (unit' ^. unit_weapon))
  Just <$> renderText font panelStyle (panelPos winSize 0 3)
    (describeStatus (unit' ^. unit_status))

renderHousePanel
  :: (ReflexSDL2 t m, MonadReader Renderer m)
  => Font -> Unit -> V2 CInt -> m (Maybe ImageSettings)
renderHousePanel font unit' winSize = do
  void $ renderText font panelHeaderStyle (panelPos winSize 0 0) "House"
  Just <$> renderText font panelStyle (panelPos winSize 0 1)
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
  => Font -> Dynamic t (V2 CInt) -> Dynamic t Word64 -> Dynamic t Bool -> ShopContent -> m (Event t ShopAction)
renderShopPanel font winSizeDyn moneyDyn hasRoomDyn content = mdo
  -- Header
  void $ image =<< holdDyn Nothing =<< dynView ((\ws -> Just <$> renderText font panelHeaderStyle (panelPos ws 0 0) "Shop") <$> winSizeDyn)

  -- Shop slots
  let slots :: [(Word8, ShopContent -> Maybe ShopItem)]
      slots = [(0, slot1), (1, slot2), (2, slot3)]

  slotEvts <- forM slots $ \(idx, slotAccessor) ->
    fmap ((idx, slotAccessor) <$) $
      image =<< holdDyn Nothing =<< dynView (renderShopSlot font content idx slotAccessor <$> winSizeDyn <*> selDyn)

  selDyn <- accumDyn toggleSlot initialSelection $ leftmost slotEvts

  -- Purchase button
  purchaseSurface <- allocateText font panelStyle "Purchase"
  let purchasePosDyn = (\ws -> surfaceToSettings purchaseSurface (panelPos ws 200 0)) <$> winSizeDyn
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
  let exitPosDyn = (\ws -> surfaceToSettings exitSurface (panelPos ws 200 2)) <$> winSizeDyn
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
    Nothing   -> Just <$> renderText font panelStyle (panelPos winSize 0 lineIdx) "-"
    Just item -> Just <$> renderText font style (panelPos winSize 0 lineIdx) (itemDescription item)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
