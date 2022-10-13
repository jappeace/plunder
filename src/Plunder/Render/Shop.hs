{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Plunder.Render.Shop(
  renderShop
  ) where

import Data.Text(Text)
import Data.Set.Lens
import Foreign.C.Types(CInt)
import Witherable(catMaybes)
import Data.Word(Word8, Word64)
import Plunder.Shop
import Plunder.Render.Text
import Plunder.Render.Color
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Font
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)

newtype ShopState = MkShopState { selectedItems :: Map Word8 (ShopContent -> (Maybe ShopItem)) }

initialState :: ShopState
initialState = MkShopState mempty

renderShop :: ReflexSDL2 t m
    => DynamicWriter t [Layer m] m
    => MonadReader Renderer m
    => Font -> Dynamic t (Maybe ShopContent) -> Dynamic t Word64 -> m (Event t ShopAction)
renderShop font shopContent playerMoney = mdo
  renderer <- ask

  commitLayer $ renderShopBackground renderer <$> shopContent
  shopSurface <- allocateText font shopStyle "Shop"
  void $ image $ shopContent & mapped._Just .~ surfaceToSettings shopSurface (shopPosition 0)

  let slots = [slot1, slot2, slot3]
  evts <- itraverse (renderSlot font shopContent shopState) slots
  shopState <- accumDyn updateState initialState $ leftmost evts

  purchaseClick <- imageEvt =<< dynView ((renderText font shopStyle (shopPosition 6) "Purchase") <$ shopContent)

  let (purchaseError, purchaseSuccess) = fanEither
                  $ current (purchaseAction <$> playerMoney <*> shopState <*> shopContent)
                  <@ purchaseClick

  small <- smallFont
  void $ image =<< holdView (pure Nothing)
               (fmap Just . renderText small shopStyle (shopPosition 5) . renderError <$> purchaseError)


  exitClick <- imageEvt =<< dynView ((renderText font shopStyle (shopPosition 7) "Exit") <$ shopContent)

  let result = leftmost [MkBought <$> purchaseSuccess,  MkExited <$ exitClick]


  performEvent_ $ ffor result $ liftIO . print . ("xxx " <>) . show

  pure $ result

data PurchaseError = ShopClosed
                   | NotEnoughMoney
                   | NoItemsSelected

renderError :: PurchaseError -> Text
renderError = \case
  ShopClosed -> "Shop closed"
  NotEnoughMoney -> "Insufficient money"
  NoItemsSelected -> "Nothing selected"

-- | on purchase one of two things will happen
-- 1. player doesn't have not enough cash, so we display a message
-- 2. player has enough cash, so cash get's subtracted and items moved to the inventory
--    we do this in update gamestate to, we just emit the items from the shop.
--    it's more convenient to do the cash check here because it
--    localises displaying the message.
purchaseAction :: Word64 -> ShopState -> Maybe ShopContent -> Either PurchaseError Haul
purchaseAction playerMoney state = \case
  Nothing -> Left ShopClosed
  Just content ->
    let
      counter :: Set ShopItem
      counter = setOf (traversed . to (\ fun -> fun content) . traversed) $ selectedItems state

      price = sumOf (folded . si_priceLens) counter
    in
      if | length counter < 1 -> Left NoItemsSelected
         | playerMoney < price -> Left NotEnoughMoney
         | True -> Right $ MkHaul
            { haulItems = counter
              , haulNewMoney = playerMoney - price
            }

renderSlot :: (MonadReader Renderer m, ReflexSDL2 t m, DynamicWriter t [Layer m] m) => Font -> Dynamic t (Maybe ShopContent) -> Dynamic t ShopState -> Int -> (ShopContent -> Maybe ShopItem) -> m (Event t (Word8, (ShopContent -> Maybe ShopItem)))
renderSlot font shopContent shopState idx' slot =
  fmap ((idx, slot) <$) $
    imageEvt . catMaybes =<< dynView (renderItem font idx . fmap slot <$> shopContent <*> shopState)
  where
     idx = fromIntegral idx'

renderItem :: (ReflexSDL2 t m, MonadReader Renderer m) => Font -> Word8 -> Maybe (Maybe ShopItem) -> ShopState -> m (Maybe ImageSettings)
renderItem font idx content state =
  traverse (renderShopItem font style (idx + 2)) content

  where
    style = if Map.member idx $ selectedItems state then shopSelectedStyle else
      shopStyle


updateState :: ShopState -> (Word8, (ShopContent -> Maybe ShopItem)) -> ShopState
updateState set'' (input, slot) = MkShopState $
  if Map.member input thisSet then
    Map.delete input thisSet
  else
    Map.insert input slot thisSet
  where
    thisSet = selectedItems set''

shopStyle :: Style
shopStyle = defaultStyle & styleColorLens .~ (V4 0 0 0 255)

shopSelectedStyle :: Style
shopSelectedStyle = shopStyle & styleColorLens .~ (V4 200 30 30 255)

renderShopBackground :: MonadIO m
    => Renderer -> Maybe ShopContent -> m ()
renderShopBackground renderer mshop = do
  setDrawColor renderer $ V4 200 200 200 255
  void $ forM_ mshop $ \_content -> do
    fillRect renderer (Just (Rectangle (P $ V2 20 20) (V2 200 200)))

shopPosition :: Word8 -> Point V2 CInt
shopPosition offset = P $ V2 30 (20 + fromIntegral offset * 20)

renderShopItem ::
  (ReflexSDL2 t m
  , MonadReader Renderer m) =>
  Font -> Style -> Word8 -> Maybe ShopItem -> m ImageSettings
renderShopItem font style offset = \case
  Nothing ->
      renderText font shopStyle position "-"
  Just item ->
      renderText font style position $ itemDescription item

  where
    position = shopPosition offset


