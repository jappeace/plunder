{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render(renderState) where

import Plunder.Shop
import Plunder.Render.Text
import           Plunder.Combat
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..)
                                     )
import           Plunder.Render.RenderFun (RenderFun(..))
import           Foreign.C.Types      (CInt)
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           Data.Bool
import           Data.Foldable
import           Data.Monoid
import           Plunder.Grid
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Arrow
import           Plunder.Render.Health
import           Plunder.Render.Hexagon
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Terrain
import           Plunder.State
import           Plunder.Render.Font

renderState :: ReflexSDL2 t m
  => MonadReader RenderFun m
  => DynamicWriter t [Layer m] m
  => Font ->  Dynamic t GameState -> m ()
renderState font state = do
  rf <- ask

  -- Terrain fill is the very first (lowest) layer: coloured hexagons for
  -- every coordinate in range, with Water used for anything outside the grid.
  renderTerrain rf (view game_board <$> state)

  vikingF <- renderImage <$> loadViking
  enemyF <- renderImage <$> loadEnemy
  loadBloodF <- renderImage <$> loadBlood
  houseF <- renderImage <$> loadHouse
  burnedHouseF <- renderImage <$> burndedHouse
  loadShopF <- renderImage <$> loadShop

  axeF <- fmap renderWeapon . renderImage <$> loadAxe
  swordF <- fmap renderWeapon . renderImage <$> loadSword
  loadF <- fmap renderWeapon . renderImage <$> loadBow

  let renderOrduning =
            [ applyImage loadBloodF $ tile_background . _Just . _Blood
            , applyImage burnedHouseF $ tile_background . _Just . _BurnedHouse
            , applyImage enemyF $ tile_content . _Just . _Enemy
            , applyImage vikingF $ tile_content . _Just . _Player
            , applyImage houseF $ tile_content . _Just . _House
            , applyImage loadShopF $ tile_content . _Just . _Shop
            , applyImage swordF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Sword
            , applyImage loadF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Bow
            , applyImage axeF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Axe
            ]

  void $ listWithKey (view game_board <$> state) $ \axial _ -> do
    hexagon $ renderHex font axial

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn -> do
    traverse_ (\fun -> fun axial tileDyn) renderOrduning
    healthBar tileDyn

  -- Selection outline drawn last so it sits on top of unit sprites
  void $ holdView (pure ())
       $ hexagon . renderSelected font <$> mapMaybe (view game_selected) (updated state)

  -- Draw planned-move arrows on top of units
  commitLayer $ ffor (view game_planned_moves <$> state) $ \plans ->
    for_ (Map.toList plans) $ \(src, path) ->
      drawPathArrows rf axialToPixel src path (V4 255 165 0 255)

  -- Fog of war overlay (covers terrain, sprites and arrows, but not HUD)
  renderFogOverlay rf state

  let moneyStyle :: Style
      moneyStyle = defaultStyle & styleColorLens .~ V4 255 215 0 255
      moneyBgRect :: Rectangle CInt
      moneyBgRect = Rectangle (P $ V2 492 4) (V2 142 28)
  commitLayer $ pure $ do
    rf_setDrawColor rf (V4 0 0 0 200)
    rf_fillRect rf (Just moneyBgRect)
  void $ imageEvt =<< dynView (state <&>
    \state' ->
      renderText font moneyStyle (P $ V2 500 10)
          ("$ " <> tshow (state' ^. game_player_inventory . inventory_money)))

  -- "Purchasing: <item>" label above the player tile when a purchase is queued
  purchaseLabelEvt <- dynView (state <&> \gs ->
    case gs ^. game_pending_purchase of
      Nothing   -> pure Nothing
      Just haul ->
        case gs ^? game_board . traversed
                  . filtered (has (tile_content . _Just . _Player))
                  . tile_coordinate of
          Nothing        -> pure Nothing
          Just playerPos ->
            let P (V2 px py) = axialToPixel playerPos
            in Just <$> renderText font defaultStyle (P $ V2 px (py - 25))
                          (describePurchase haul))
  void $ image =<< holdDyn Nothing purchaseLabelEvt


applyImage ::
  DynamicWriter t [Performable m ()] m
  => MonadReader RenderFun m
  => ReflexSDL2 t m
  => (Axial -> ImageSettings)
  -> Getting Any Tile a -- ^ condition on the tile for rendering
  -> Axial
  -> Dynamic t Tile
  ->  m ()
applyImage textureF hashPath axial tileDyn =
  void $ image $ fmap textureF <$> someSettings
  where
    someSettings = bool Nothing (Just axial)
                       . has hashPath <$> tileDyn

renderSelected :: Font -> Axial -> HexagonSettings
renderSelected font = (hexagon_color .~ V4 255 255 0 255)
               . (hexagon_is_filled .~ False)
               . (hexagon_label .~ Nothing)
               . renderHex font

describePurchase :: Haul -> T.Text
describePurchase haul =
  "Purchasing " <> T.intercalate ", " (itemTypeDescription . si_type <$> toList (haulItems haul))
