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

  let cameraDyn = view game_camera <$> state

  -- Terrain fill is the very first (lowest) layer: coloured hexagons for
  -- every coordinate in range, with Water used for anything outside the grid.
  renderTerrain rf cameraDyn (view game_board <$> state)

  vikingTex <- loadViking
  enemyTex <- loadEnemy
  bloodTex <- loadBlood
  houseTex <- loadHouse
  burnedHouseTex <- burndedHouse
  shopTex <- loadShop

  axeTex <- loadAxe
  swordTex <- loadSword
  bowTex <- loadBow

  let renderOrduning =
            [ applyImageCam cameraDyn (renderImageCam' bloodTex) $ tile_background . _Just . _Blood
            , applyImageCam cameraDyn (renderImageCam' burnedHouseTex) $ tile_background . _Just . _BurnedHouse
            , applyImageCam cameraDyn (renderImageCam' enemyTex) $ tile_content . _Just . _Enemy
            , applyImageCam cameraDyn (renderImageCam' vikingTex) $ tile_content . _Just . _Player
            , applyImageCam cameraDyn (renderImageCam' houseTex) $ tile_content . _Just . _House
            , applyImageCam cameraDyn (renderImageCam' shopTex) $ tile_content . _Just . _Shop
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam swordTex) $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Sword
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam bowTex) $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Bow
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam axeTex) $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Axe
            ]

  void $ listWithKey (view game_board <$> state) $ \axial _ -> do
    hexagonDyn $ renderHexCam <$> cameraDyn <*> pure font <*> pure axial

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn -> do
    traverse_ (\fun -> fun axial tileDyn) renderOrduning
    healthBar cameraDyn tileDyn

  -- Selection outline drawn last so it sits on top of unit sprites
  void $ holdView (pure ())
       $ (\axial -> hexagonDyn $ renderSelected <$> cameraDyn <*> pure font <*> pure axial)
         <$> mapMaybe (view game_selected) (updated state)

  -- Draw planned-move arrows on top of units
  commitLayer $ ffor2 cameraDyn (view game_planned_moves <$> state) $ \cam plans ->
    for_ (Map.toList plans) $ \(src, path) ->
      drawPathArrows rf (axialToPixelCam cam) src path (V4 255 165 0 255)

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
            let P (V2 px py) = axialToPixelCam (gs ^. game_camera) playerPos
            in Just <$> renderText font defaultStyle (P $ V2 px (py - 25))
                          (describePurchase haul))
  void $ image =<< holdDyn Nothing purchaseLabelEvt

-- | Helper to flip argument order for renderImageCam.
renderImageCam' :: Texture -> V2 CInt -> Axial -> ImageSettings
renderImageCam' tex cam = renderImageCam cam tex

-- | Camera-aware version of applyImage. The image position reacts to camera changes.
applyImageCam ::
  DynamicWriter t [Performable m ()] m
  => MonadReader RenderFun m
  => ReflexSDL2 t m
  => Dynamic t (V2 CInt)
  -> (V2 CInt -> Axial -> ImageSettings)
  -> Getting Any Tile a -- ^ condition on the tile for rendering
  -> Axial
  -> Dynamic t Tile
  ->  m ()
applyImageCam cameraDyn textureF hashPath axial tileDyn =
  void $ image someSettings
  where
    someSettings = (\cam tile ->
        if has hashPath tile
          then Just (textureF cam axial)
          else Nothing
      ) <$> cameraDyn <*> tileDyn

renderSelected :: V2 CInt -> Font -> Axial -> HexagonSettings
renderSelected cam font = (hexagon_color .~ V4 255 255 0 255)
               . (hexagon_is_filled .~ False)
               . (hexagon_label .~ Nothing)
               . renderHexCam cam font

describePurchase :: Haul -> T.Text
describePurchase haul =
  "Purchasing " <> T.intercalate ", " (itemTypeDescription . si_type <$> toList (haulItems haul))
