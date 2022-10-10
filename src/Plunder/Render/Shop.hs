{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

renderShop :: ReflexSDL2 t m
    => DynamicWriter t [Layer m] m
    => MonadReader Renderer m
    => Font -> Dynamic t (Maybe ShopContent) -> m ()
renderShop font shopDyn = do
  renderer <- ask
  commitLayer $ renderShopBackground renderer <$> shopDyn
  void $ imageEvt =<< dynView ((renderText font shopStyle (shopPosition 0) "Shop") <$ shopDyn)
  void $ imageEvt . catMaybes =<< dynView (traverse (renderShopItem font 1 . slot1) <$> shopDyn)
  void $ imageEvt . catMaybes =<< dynView (traverse (renderShopItem font 2 . slot2) <$> shopDyn)
  click <- imageEvt . catMaybes =<< dynView (traverse (renderShopItem font 3 . slot3) <$> shopDyn)
  performEvent_ $ ffor click $ liftIO . print . ("xxx " <>) . show
  pure ()

shopStyle :: Style
shopStyle = defaultStyle & styleColorLens .~ (V4 0 0 0 255)

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
  Font -> Word8 -> Maybe ShopItem -> m ImageSettings
renderShopItem font offset = \case
  Nothing ->
      renderText font shopStyle position "-"
  Just item ->
      renderText font shopStyle position $ itemDescription item

  where
    position = shopPosition offset


