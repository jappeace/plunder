{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Plunder.Render.Shop(renderShop) where

import Foreign.C.Types(CInt)
import Witherable(catMaybes)
import Data.Word(Word8)
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
import Data.Set(Set)
import qualified Data.Set as Set

newtype ShopState = MkShopState { itemSelectedIndexes :: Set Word8 }
  deriving Show

initialState :: ShopState
initialState = MkShopState mempty

renderShop :: ReflexSDL2 t m
    => DynamicWriter t [Layer m] m
    => MonadReader Renderer m
    => Font -> Dynamic t (Maybe ShopContent) -> m ()
renderShop font shopDyn = mdo
  renderer <- ask
  commitLayer $ renderShopBackground renderer <$> shopDyn
  void $ imageEvt =<< dynView ((renderText font shopStyle (shopPosition 0) "Shop") <$ shopDyn)
  let slots = [slot1, slot2, slot3]
  evts <- itraverse (renderSlot font shopDyn stateDyn) slots
  stateDyn <- accumDyn updateState initialState $ leftmost evts

  performEvent_ $ ffor (updated stateDyn) $ liftIO . print . ("xxx " <>) . show
  void $ imageEvt =<< dynView ((renderText font shopStyle (shopPosition 6) "Purchase") <$ shopDyn)
  pure ()

renderSlot :: (MonadReader Renderer m, ReflexSDL2 t m, DynamicWriter t [Layer m] m) => Font -> Dynamic t (Maybe ShopContent) -> Dynamic t ShopState -> Int -> (ShopContent -> Maybe ShopItem) -> m (Event t Word8)
renderSlot font shopContent shopState idx' slot = fmap (idx <$) $
    imageEvt . catMaybes =<< dynView (renderItem font idx . fmap slot <$> shopContent <*> shopState)
  where
     idx = fromIntegral idx'

renderItem :: (ReflexSDL2 t m, MonadReader Renderer m) => Font -> Word8 -> Maybe (Maybe ShopItem) -> ShopState -> m (Maybe ImageSettings)
renderItem font idx content state =
  traverse (renderShopItem font style (idx + 2)) content

  where
    style = if Set.member idx $ itemSelectedIndexes state then shopSelectedStyle else
      shopStyle


updateState :: ShopState -> Word8 -> ShopState
updateState set'' input = MkShopState $
  if Set.member input thisSet then
    Set.delete input thisSet
  else
    Set.insert input thisSet
  where
    thisSet = itemSelectedIndexes set''

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


