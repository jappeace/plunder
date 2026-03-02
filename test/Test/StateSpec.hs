module Test.StateSpec(spec) where

import           Control.Lens
import           Control.Monad.Trans.Random.Lazy (runRandT)
import qualified Data.Set                        as Set
import           Plunder.Shop
import           Plunder.State
import           System.Random                   (mkStdGen)
import           Test.Hspec

-- | Run a single update event against a game state (ignoring randomness)
runEvt :: UpdateEvts -> GameState -> GameState
runEvt evt gs = updateState gs (rng, evt)
  where
    rng = MkRandTNT (\inner -> fst <$> runRandT inner (mkStdGen 42))

spec :: Spec
spec = describe "Inventory" $ do
  it "starts closed" $
    initialState ^. game_inventory_open `shouldBe` False

  it "ToggleInventory opens when closed" $
    runEvt ToggleInventory initialState ^. game_inventory_open `shouldBe` True

  it "ToggleInventory closes when open" $
    runEvt ToggleInventory (runEvt ToggleInventory initialState) ^. game_inventory_open
      `shouldBe` False

  it "buying an item adds it to inventory" $ do
    let item   = MkShopItem 4 ShopHealthPotion
        haul   = MkHaul { haulItems = Set.singleton item, haulNewMoney = 0 }
        result = runEvt (ShopUpdates (MkBought haul)) initialState
    result ^. game_player_inventory . inventroy_item `shouldBe` Set.singleton item

  it "buying multiple items accumulates them" $ do
    let item1 = MkShopItem 4 ShopHealthPotion
        item2 = MkShopItem 2 ShopUnit
        haul1 = MkHaul { haulItems = Set.singleton item1, haulNewMoney = 10 }
        haul2 = MkHaul { haulItems = Set.singleton item2, haulNewMoney = 5  }
        result = runEvt (ShopUpdates (MkBought haul2))
               $ runEvt (ShopUpdates (MkBought haul1)) initialState
    result ^. game_player_inventory . inventroy_item
      `shouldBe` Set.fromList [item1, item2]
