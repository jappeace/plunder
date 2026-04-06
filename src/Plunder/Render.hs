{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render(renderState) where

import Plunder.Shop
import Plunder.Render.Text
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..)
                                     )
import           Plunder.Render.RenderFun (RenderFun(..))
import           Foreign.C.Types      (CInt)
import           Data.Foldable
import           Data.Monoid
import           Plunder.Grid (Axial, axialToPixelCam)
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Arrow
import           Plunder.Render.Health
import           Plunder.Render.Hexagon
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.Render.Terrain
import           Plunder.RenderState
import           Plunder.Render.Font

renderState :: ReflexSDL2 t m
  => MonadReader RenderFun m
  => DynamicWriter t [Layer m] m
  => Font -> Dynamic t RenderState -> m ()
renderState font rstate = do
  rf <- ask

  let cameraDyn = view render_camera <$> rstate
      tilesDyn  = view render_tiles <$> rstate

  -- Terrain fill is the very first (lowest) layer: coloured hexagons for
  -- every coordinate in range, with Water used for anything outside the grid.
  renderTerrain rf cameraDyn tilesDyn

  vikingTex <- loadViking
  enemyTex <- loadEnemy
  bloodTex <- loadBlood
  houseTex <- loadHouse
  burnedHouseTex <- burndedHouse
  shopTex <- loadShop
  boatTex <- loadBoat

  axeTex <- loadAxe
  swordTex <- loadSword
  bowTex <- loadBow

  let renderOrduning =
            [ applyImageCam cameraDyn (renderImageCam' bloodTex) $ rtile_background . _Just . only BloodSplash
            , applyImageCam cameraDyn (renderImageCam' burnedHouseTex) $ rtile_background . _Just . only BurnedHouseBg
            , applyImageCam cameraDyn (renderImageCam' burnedHouseTex) $ rtile_background . _Just . only BurnedShopBg
            , applyImageCam cameraDyn (renderImageCam' enemyTex) $ rtile_sprite . _Just . only EnemySprite
            , applyImageCam cameraDyn (renderImageCam' vikingTex) $ rtile_sprite . _Just . only PlayerSprite
            , applyImageCam cameraDyn (renderImageCam' houseTex) $ rtile_sprite . _Just . only HouseSprite
            , applyImageCam cameraDyn (renderImageCam' shopTex) $ rtile_sprite . _Just . only ShopSprite
            , applyImageCam cameraDyn (renderImageCam' boatTex) $ rtile_sprite . _Just . only BoatSprite
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam swordTex) $ rtile_weapon . _Just . only SwordSprite
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam bowTex) $ rtile_weapon . _Just . only BowSprite
            , applyImageCam cameraDyn (\cam -> renderWeapon . renderImageCam cam axeTex) $ rtile_weapon . _Just . only AxeSprite
            ]

  void $ listWithKey tilesDyn $ \axial _ -> do
    hexagonDyn $ renderHexCam <$> cameraDyn <*> pure font <*> pure axial

  -- simple list doesn't cache on key change
  void $ listWithKey tilesDyn $ \axial tileDyn -> do
    traverse_ (\fun -> fun axial tileDyn) renderOrduning
    healthBar cameraDyn tileDyn

  -- Selection outline drawn last so it sits on top of unit sprites
  void $ holdView (pure ())
       $ (\axial -> hexagonDyn $ renderSelected <$> cameraDyn <*> pure font <*> pure axial)
         <$> mapMaybe (view render_selectedTile) (updated rstate)

  -- Draw planned-move arrows on top of units
  commitLayer $ ffor2 cameraDyn (view render_plannedArrows <$> rstate) $ \cam arrows ->
    for_ arrows $ \arrow ->
      drawPathArrows rf (axialToPixelCam cam)
        (arrow ^. arrow_source) (arrow ^. arrow_waypoints) (V4 255 165 0 255)

  -- Fog of war overlay (covers terrain, sprites and arrows, but not HUD)
  renderFogOverlay rf cameraDyn tilesDyn

  let moneyStyle :: Style
      moneyStyle = defaultStyle & styleColorLens .~ V4 255 215 0 255
      moneyBgRect :: Rectangle CInt
      moneyBgRect = Rectangle (P $ V2 492 4) (V2 142 28)
  commitLayer $ pure $ do
    rf_setDrawColor rf (V4 0 0 0 200)
    rf_fillRect rf (Just moneyBgRect)
  void $ imageEvt =<< dynView (rstate <&>
    \rs ->
      renderText font moneyStyle (P $ V2 500 10)
          ("$ " <> tshow (rs ^. render_hud . hud_money . money_amount)))

  -- "Purchasing: <item>" label above the player tile when a purchase is queued
  purchaseLabelEvt <- dynView (rstate <&> \rs ->
    case rs ^. render_hud . hud_purchaseLabel of
      Nothing    -> pure Nothing
      Just plabel ->
        let P (V2 px py) = axialToPixelCam (rs ^. render_camera) (plabel ^. plabel_playerPos)
        in Just <$> renderText font defaultStyle (P $ V2 px (py - 25))
                      (plabel ^. plabel_text))
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
  -> Getting Any RenderTile a -- ^ condition on the tile for rendering
  -> Axial
  -> Dynamic t RenderTile
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
