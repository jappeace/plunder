{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render.Inventory(renderInventory) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Plunder.Render.RenderFun (RenderFun(..))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Foreign.C.Types      (CInt)
import           Plunder.Render.Font
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Text
import           Plunder.Shop
import           Reflex
import           Reflex.SDL2

maxSlots :: Int
maxSlots = 8

renderInventory
  :: ReflexSDL2 t m
  => DynamicWriter t [Layer m] m
  => MonadReader RenderFun m
  => Font -> Dynamic t Bool -> Dynamic t (Set ShopItem) -> m (Event t ShopItem)
renderInventory font isOpen items = do
  rf <- ask

  commitLayer $ renderInventoryBackground rf <$> isOpen

  titleSurface <- allocateText font inventoryStyle "Inventory"
  void $ image $
    (\open -> if open then Just (surfaceToSettings titleSurface (invPosition 0)) else Nothing)
    <$> isOpen

  let itemListDyn = padTo maxSlots . Set.toList <$> items
  evts <- forM [0 .. maxSlots - 1] $ \idx -> do
    let slotDyn = (!! idx) <$> itemListDyn
    clicks <- image =<< holdDyn Nothing =<<
      dynView (renderSlot font idx <$> isOpen <*> slotDyn)
    pure $ fmapMaybe id $ current slotDyn <@ clicks
  pure (leftmost evts)

padTo :: Int -> [a] -> [Maybe a]
padTo n xs =
  let taken = take n xs
  in map Just taken ++ replicate (n - length taken) Nothing

renderSlot
  :: (ReflexSDL2 t m, MonadReader RenderFun m)
  => Font -> Int -> Bool -> Maybe ShopItem -> m (Maybe ImageSettings)
renderSlot _ _ False _ = pure Nothing
renderSlot font idx True mItem =
  Just <$> case mItem of
    Nothing   -> renderText font inventoryStyle (invPosition (idx + 1)) "-"
    Just item -> renderText font inventoryStyle (invPosition (idx + 1)) (itemTypeDescription (si_type item))

inventoryStyle :: Style
inventoryStyle = defaultStyle & styleColorLens .~ V4 0 0 0 255

invPosition :: Int -> Point V2 CInt
invPosition offset = P $ V2 250 (20 + fromIntegral offset * 20)

renderInventoryBackground :: MonadIO m => RenderFun -> Bool -> m ()
renderInventoryBackground rf open = do
  rf_setDrawColor rf $ V4 200 200 200 255
  when open $
    rf_fillRect rf (Just (Rectangle (P $ V2 240 20) (V2 200 200)))
