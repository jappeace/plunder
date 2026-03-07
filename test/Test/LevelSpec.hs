module Test.LevelSpec
  ( spec
  )
where

import Control.Lens
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import Plunder.Grid
import Plunder.Level
import Plunder.State
import Test.Hspec

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
      -- board equality (tile contents + backgrounds)
      (gs ^. game_board) `shouldBe` (gs' ^. game_board)

    it "initialState matches levelToGameState defaultLevel" $ do
      let gs = levelToGameState defaultLevel
      (initialState ^. game_board) `shouldBe` (gs ^. game_board)
