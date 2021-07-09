{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Render(renderState) where

import Data.Monoid
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader  (MonadReader (..))
import           Data.Bool
import           Grid
import           Hexagon
import           Image
import           Layer
import           Reflex
import           Reflex.SDL2
import           State
import Data.Foldable
import Combat

renderState :: ReflexSDL2 t m
  => MonadReader Renderer m
  => DynamicWriter t [Layer m] m
  => Dynamic t GameState -> m ()
renderState state = do
  vikingF <- renderImage <$> loadViking
  enemyF <- renderImage <$> loadEnemy
  loadBloodF <- renderImage <$> loadBlood

  axeF <- fmap renderWeapon . renderImage <$> loadAxe
  swordF <- fmap renderWeapon . renderImage <$> loadSword
  loadF <- fmap renderWeapon . renderImage <$> loadBow

  let renderOrduning =
            [ applyImage loadBloodF $ tile_background . _Just . _Blood
            , applyImage enemyF $ tile_content . _Just . _Enemy
            , applyImage vikingF $ tile_content . _Just . _Player
            , applyImage swordF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Sword
            , applyImage loadF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Bow
            , applyImage axeF $ tile_content . _Just . tc_unit . unit_weapon . _Just . _Axe
            ]

  void $ listWithKey (view game_board <$> state) $ \axial _ -> do
    hexagon $ renderHex axial

  void $ holdView (pure ())
       $ hexagon . renderSelected <$> mapMaybe (view game_selected) (updated state)

  -- simple list doesn't cache on key change
  void $ listWithKey (view game_board <$> state) $ \axial tileDyn ->
    traverse_ (\fun -> fun axial tileDyn) renderOrduning

applyImage ::
  DynamicWriter t [Performable m ()] m
  => MonadReader Renderer m
  => ReflexSDL2 t m
  => (Axial -> ImageSettings)
  -> (Getting Any Tile a)
  -> Axial
  -> Dynamic t Tile
  ->  m ()
applyImage textureF hashPath axial tileDyn =
  image $ fmap textureF <$> someSettings
  where
    someSettings = bool Nothing (Just axial)
                       . has hashPath <$> tileDyn

  


renderSelected :: Axial -> HexagonSettings
renderSelected = (hexagon_color .~ V4 255 128 128 255)
               . (hexagon_is_filled .~ True)
               . renderHex
