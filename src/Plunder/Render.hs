{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plunder.Render(renderState) where

import Plunder.Render.Shop
import Plunder.Shop
import Plunder.Render.Text
import           Plunder.Combat
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (MonadReader (..))
import           Data.Bool
import           Data.Foldable
import           Data.Monoid
import           Plunder.Grid
import           Reflex
import           Reflex.SDL2
import           Plunder.Render.Health
import           Plunder.Render.Hexagon
import           Plunder.Render.Image
import           Plunder.Render.Layer
import           Plunder.State
import           Plunder.Render.Font

renderState :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Dynamic t GameState -> m ()
renderState state = do
  vikingF <- renderImage <$> loadViking
  enemyF <- renderImage <$> loadEnemy
  loadBloodF <- renderImage <$> loadBlood
  houseF <- renderImage <$> loadHouse
  burnedHouseF <- renderImage <$> burndedHouse
  loadShopF <- renderImage <$> loadShop
  font <- defaultFont

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

  void $ holdView (pure ())
       $ hexagon . renderSelected font <$> mapMaybe (view game_selected) (updated state)

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn -> do
    traverse_ (\fun -> fun axial tileDyn) renderOrduning
    healthBar tileDyn

  imageEvt =<< dynView (state <&>
    \state' ->
      renderText font defaultStyle (P $ V2 500 10)
          ("Money " <> tshow (state' ^. game_player_inventory . inventory_money)))

  renderShop font $ view game_shop <$> state

applyImage ::
  DynamicWriter t [Performable m ()] m
  => MonadReader Renderer m
  => ReflexSDL2 t m
  => (Axial -> ImageSettings)
  -> Getting Any Tile a -- ^ condition on the tile for rendering
  -> Axial
  -> Dynamic t Tile
  ->  m ()
applyImage textureF hashPath axial tileDyn =
  image $ fmap textureF <$> someSettings
  where
    someSettings = bool Nothing (Just axial)
                       . has hashPath <$> tileDyn

renderSelected :: Font -> Axial -> HexagonSettings
renderSelected font = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex font
