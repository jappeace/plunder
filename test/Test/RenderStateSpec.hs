module Test.RenderStateSpec(spec) where

import           Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Foreign.C.Types (CInt)
import           SDL.Vect (V2(..))
import           Plunder.Combat (Weapon(..), defUnit, maxHealth, unit_hp, unit_weapon)
import           Plunder.Grid
import           Plunder.Level (defaultLevel)
import           Plunder.RenderState
import           Plunder.Shop
import           Plunder.State
import           Test.Hspec

-- | Default initial state from the default level.
testState :: GameState
testState = initialState

-- | Player position in the default level (MkAxial 2 3).
playerAxial :: Axial
playerAxial = MkAxial 2 3

-- | An axial coordinate far from any player, expected to be Unexplored.
farAxial :: Axial
farAxial = MkAxial 6 6

-- | Build a render state with no banner and no help overlay.
defaultRender :: GameState -> RenderState
defaultRender = gameStateToRenderState 0 False

spec :: Spec
spec = do
  describe "tileContentToSprite" $ do
    it "maps Player to PlayerSprite" $
      tileContentToSprite (Player defUnit) `shouldBe` PlayerSprite

    it "maps Enemy to EnemySprite" $
      tileContentToSprite (Enemy defUnit) `shouldBe` EnemySprite

    it "maps House to HouseSprite" $
      tileContentToSprite (House defUnit) `shouldBe` HouseSprite

    it "maps Boat to BoatSprite" $
      tileContentToSprite Boat `shouldBe` BoatSprite

  describe "tileContentToWeapon" $ do
    it "returns SwordSprite for a unit with Sword" $
      tileContentToWeapon (Player (defUnit & unit_weapon ?~ Sword)) `shouldBe` Just SwordSprite

    it "returns BowSprite for a unit with Bow" $
      tileContentToWeapon (Enemy (defUnit & unit_weapon ?~ Bow)) `shouldBe` Just BowSprite

    it "returns AxeSprite for a unit with Axe" $
      tileContentToWeapon (Player (defUnit & unit_weapon ?~ Axe)) `shouldBe` Just AxeSprite

    it "returns Nothing for content without a weapon" $
      tileContentToWeapon (Player defUnit) `shouldBe` Nothing

    it "returns Nothing for Boat" $
      tileContentToWeapon Boat `shouldBe` Nothing

  describe "backgroundToSprite" $ do
    it "maps Blood to BloodSplash" $
      backgroundToSprite Blood `shouldBe` BloodSplash

    it "maps BurnedHouse to BurnedHouseBg" $
      backgroundToSprite BurnedHouse `shouldBe` BurnedHouseBg

    it "maps BurnedShop to BurnedShopBg" $
      backgroundToSprite BurnedShop `shouldBe` BurnedShopBg

  describe "gameStateToRenderState" $ do
    let rs = defaultRender testState

    it "tiles near the player are Visible" $
      let tile = rs ^? render_tiles . ix playerAxial
      in (tile ^? _Just . rtile_visibility) `shouldBe` Just Visible

    it "far tiles are Unexplored or Fog" $
      let vis = rs ^? render_tiles . ix farAxial . rtile_visibility
      in vis `shouldSatisfy` \v -> v == Just Unexplored || v == Just Fog

    it "player tile has PlayerSprite" $
      (rs ^? render_tiles . ix playerAxial . rtile_sprite) `shouldBe` Just (Just PlayerSprite)

    it "health bar present for player tile" $
      let hb = rs ^? render_tiles . ix playerAxial . rtile_healthBar . _Just
      in do
        (hb ^? _Just . hb_currentHp) `shouldBe` Just maxHealth
        (hb ^? _Just . hb_maxHp) `shouldBe` Just maxHealth

    it "empty tile has no sprite" $
      -- MkAxial 0 0 should be empty in the default level
      let emptyAxial = MkAxial 0 0
      in (rs ^? render_tiles . ix emptyAxial . rtile_sprite) `shouldBe` Just Nothing

    it "selected tile is preserved" $
      let gs = testState & game_selected .~ Just playerAxial
          rs' = defaultRender gs
      in rs' ^. render_selectedTile `shouldBe` Just playerAxial

    it "camera offset is preserved" $
      let gs = testState & game_camera .~ V2 100 200
          rs' = defaultRender gs
      in rs' ^. render_camera `shouldBe` V2 100 200

    it "planned moves produce PlannedArrow values" $
      let gs = testState & game_planned_moves .~ Map.singleton playerAxial [MkAxial 3 3, MkAxial 4 3]
          rs' = defaultRender gs
      in rs' ^. render_plannedArrows `shouldBe`
           [MkPlannedArrow playerAxial [MkAxial 3 3, MkAxial 4 3]]

    it "money display reflects inventory" $
      (rs ^. render_hud . hud_money . money_amount) `shouldBe`
        (testState ^. game_player_inventory . inventory_money)

    it "inventory closed by default" $
      (rs ^. render_hud . hud_inventory . invInfo_isOpen) `shouldBe` False

    it "inventory items are empty by default" $
      (rs ^. render_hud . hud_inventory . invInfo_items) `shouldBe` Set.empty

  describe "BannerState" $ do
    it "Playing phase produces NoBanner" $
      let rs = gameStateToRenderState 100 False testState
      in rs ^. render_hud . hud_banner `shouldBe` NoBanner

    it "YouDied phase produces DeathBanner with alpha" $
      let gs = testState & game_phase .~ YouDied
          rs = gameStateToRenderState 150 False gs
      in rs ^. render_hud . hud_banner `shouldBe` DeathBanner 150

    it "YouVictorious phase produces VictoryBanner with alpha" $
      let gs = testState & game_phase .~ YouVictorious
          rs = gameStateToRenderState 200 False gs
      in rs ^. render_hud . hud_banner `shouldBe` VictoryBanner 200

  describe "HelpState" $ do
    it "helpOpen True produces HelpVisible" $
      let rs = gameStateToRenderState 0 True testState
      in rs ^. render_hud . hud_help `shouldBe` HelpVisible

    it "helpOpen False produces HelpHidden" $
      let rs = gameStateToRenderState 0 False testState
      in rs ^. render_hud . hud_help `shouldBe` HelpHidden

  describe "ContextInfo" $ do
    it "no selection produces ContextNone" $
      let rs = defaultRender testState
      in rs ^. render_hud . hud_contextPanel `shouldBe` ContextNone

    it "selecting the player tile produces ContextPlayer" $
      let gs = testState & game_selected .~ Just playerAxial
          rs = defaultRender gs
          ctx = rs ^. render_hud . hud_contextPanel
      in case ctx of
        ContextPlayer {} -> pure ()
        other            -> expectationFailure $ "Expected ContextPlayer, got: " <> show other
