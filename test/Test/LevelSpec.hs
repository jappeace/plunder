{-# OPTIONS_GHC -Wno-orphans #-}

module Test.LevelSpec
  ( spec
  )
where

import Control.Lens hiding (Level, elements)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import Plunder.Combat
import Plunder.Grid
import Plunder.Level
import Plunder.Shop
import Plunder.State
import Test.Hspec
import Test.QuickCheck

-- Custom generators that produce values safe for TOML roundtripping.
-- We can't redefine Arbitrary for types that already have instances
-- (Background, ShopItem, etc.) so we use explicit generators.

genBackground :: Gen Background
genBackground = elements [Blood, BurnedHouse, BurnedShop]

genShopItemType :: Gen ShopItemType
genShopItemType = oneof
  [ ShopWeapon <$> arbitrary
  , pure ShopUnit
  , pure ShopHealthPotion
  ]

-- | Prices must fit in TOML's signed 64-bit integer range
genShopItem :: Gen ShopItem
genShopItem = MkShopItem <$> choose (0, 1000) <*> genShopItemType

genShopContent :: Gen ShopContent
genShopContent = MkShopContent
  <$> frequency [(3, Just <$> genShopItem), (1, pure Nothing)]
  <*> frequency [(3, Just <$> genShopItem), (1, pure Nothing)]
  <*> frequency [(3, Just <$> genShopItem), (1, pure Nothing)]

genTileContentDef :: Gen TileContentDef
genTileContentDef = oneof
  [ PlayerDef <$> choose (1, 100) <*> arbitrary
  , EnemyDef  <$> choose (1, 100) <*> arbitrary
  , HouseDef  <$> choose (1, 100)
  , ShopDef   <$> genShopContent
  ]

genTilePlacement :: Gen TilePlacement
genTilePlacement = do
  q <- choose (-50, 50)
  r <- choose (-50, 50)
  -- at least one of content/background/terrain must be present
  hasContent <- arbitrary
  hasBg      <- if hasContent then arbitrary else pure True
  content    <- if hasContent then Just <$> genTileContentDef else pure Nothing
  bg         <- if hasBg then Just <$> genBackground else pure Nothing
  terrain    <- frequency [(1, Just <$> elements [Land, Water, Mountains]), (3, pure Nothing)]
  pure $ MkTilePlacement q r content bg terrain

genLevel :: Gen Level
genLevel = do
  begin <- choose (-10, 10)
  end   <- choose (begin, begin + 20)
  money <- choose (0, 1000)
  tiles <- listOf1 genTilePlacement
  pure $ MkLevel begin end money tiles

prop_tomlRoundtrip :: Property
prop_tomlRoundtrip = forAll genLevel $ \lvl ->
  counterexample ("TOML output:\n" <> show (levelToToml lvl)) $
    decodeLevel (levelToToml lvl) === Right lvl

spec :: Spec
spec = do
  describe "Level TOML roundtripping" $ do
    it "parse then print then parse yields same Level" $ do
      let toml = levelToToml defaultLevel
      let reparsed = decodeLevel toml
      reparsed `shouldBe` Right defaultLevel

    it "roundtrips the current level1.toml file" $ do
      contents <- TIO.readFile "assets/levels/level1.toml"
      let parsed = decodeLevel contents
      case parsed of
        Left err -> expectationFailure (show err)
        Right lvl -> do
          let reencoded = levelToToml lvl
          decodeLevel reencoded `shouldBe` Right lvl

    it "levelToGameState produces correct grid dimensions" $ do
      let gs = levelToGameState defaultLevel
      Map.size (gs ^. game_board) `shouldBe` 49  -- 7x7 grid

    it "levelToGameState places player at correct position" $ do
      let gs = levelToGameState defaultLevel
      has (game_board . at (MkAxial 2 3) . _Just . tile_content . _Just . _Player) gs
        `shouldBe` True

    it "levelToGameState places enemies at correct positions" $ do
      let gs = levelToGameState defaultLevel
      has (game_board . at (MkAxial 4 5) . _Just . tile_content . _Just . _Enemy) gs
        `shouldBe` True
      has (game_board . at (MkAxial 4 4) . _Just . tile_content . _Just . _Enemy) gs
        `shouldBe` True
      has (game_board . at (MkAxial 4 3) . _Just . tile_content . _Just . _Enemy) gs
        `shouldBe` True
      has (game_board . at (MkAxial 0 6) . _Just . tile_content . _Just . _Enemy) gs
        `shouldBe` True

    it "levelToGameState places blood background at correct position" $ do
      let gs = levelToGameState defaultLevel
      gs ^? game_board . at (MkAxial 1 6) . _Just . tile_background . _Just . _Blood
        `shouldBe` Just ()

    it "gameStateToLevel roundtrips through levelToGameState" $ do
      let gs = levelToGameState defaultLevel
          lvl' = gameStateToLevel 0 6 gs
          gs'  = levelToGameState lvl'
      (gs ^. game_board) `shouldBe` (gs' ^. game_board)

    it "initialState matches levelToGameState defaultLevel" $ do
      let gs = levelToGameState defaultLevel
      (initialState ^. game_board) `shouldBe` (gs ^. game_board)

    it "arbitrary levels roundtrip through TOML encode/decode" $
      property prop_tomlRoundtrip
