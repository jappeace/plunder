module Test.StateSpec(spec) where

import           Control.Lens
import           Control.Monad.Trans.Random.Lazy (runRandT)
import qualified Data.Set                        as Set
import           Plunder.Combat (Weapon(..))
import           Plunder.Grid
import           Plunder.Shop
import           Plunder.State
import           System.Random                   (mkStdGen)
import           Test.Hspec
import           Test.QuickCheck                 (property)

-- | Run a single update event against a game state (ignoring randomness)
runEvt :: UpdateEvts -> GameState -> GameState
runEvt evt gs = updateState gs (rng, evt)
  where
    rng = MkRandTNT (\inner -> fst <$> runRandT inner (mkStdGen 42))

-- | The shop tile placed by the level function
shopAxial :: Axial
shopAxial = MkAxial 2 6

shopTileContent :: ShopContent
shopTileContent = MkShopContent (Just (MkShopItem 4 ShopHealthPotion)) (Just (MkShopItem 8 ShopUnit)) Nothing

-- | A state where the player is on an adjacent tile and selected
playerAdjacentToShop :: GameState
playerAdjacentToShop = initialState
  & game_selected .~ Just (MkAxial 2 5)
  & game_board . at (MkAxial 2 5) . _Just . tile_content ?~ Player defUnit

spec :: Spec
spec = do
 describe "Shop" $ do
  it "starts closed" $
    initialState ^. game_shop `shouldBe` Nothing

  it "right-clicking shop while adjacent opens the shop" $
    runEvt (RightClick shopAxial) playerAdjacentToShop ^. game_shop
      `shouldBe` Just shopTileContent

  it "right-clicking shop from far away does not open it" $
    -- player at MkAxial 2 3 is not adjacent to shop at MkAxial 2 6
    runEvt (RightClick shopAxial) (initialState & game_selected .~ Just (MkAxial 2 3)) ^. game_shop
      `shouldBe` Nothing

  it "exiting the shop closes it" $
    runEvt (ShopUpdates MkExited) (set game_shop (Just shopTileContent) initialState) ^. game_shop
      `shouldBe` Nothing

 describe "Friend spawning" $ do
  it "buying a friend spawns a second Player on the board" $ do
    let haul   = MkHaul { haulItems = Set.singleton (MkShopItem 0 ShopUnit), haulNewMoney = 0 }
        result = runEvt (ShopUpdates (MkBought haul)) initialState
        players = result ^.. game_board . traversed . tile_content . _Just . _Player
    length players `shouldBe` 2

  it "buying a friend does not add it to the inventory" $ do
    let haul   = MkHaul { haulItems = Set.singleton (MkShopItem 0 ShopUnit), haulNewMoney = 0 }
        result = runEvt (ShopUpdates (MkBought haul)) initialState
    result ^. game_player_inventory . inventroy_item `shouldBe` Set.empty

  it "findFreeAdjacent returns Just when an adjacent tile is free" $
    findFreeAdjacent initialState `shouldNotBe` Nothing

  it "findFreeAdjacent returns Nothing when all adjacent tiles are occupied" $ do
    let playerAxial  = MkAxial 2 3
        blockedState = foldl (\gs ax -> gs & game_board . ix ax . tile_content ?~ Enemy defUnit)
                             initialState
                             (neigbours playerAxial)
    findFreeAdjacent blockedState `shouldBe` Nothing

  it "spawned friend is adjacent to the player" $ do
    let haul         = MkHaul { haulItems = Set.singleton (MkShopItem 0 ShopUnit), haulNewMoney = 0 }
        result       = runEvt (ShopUpdates (MkBought haul)) initialState
        playerAxial  = MkAxial 2 3
        friendAxials = result ^.. game_board
                                . itraversed
                                . filtered (has (tile_content . _Just . _Player))
                                . tile_coordinate
        -- remove the original player, leaving only the spawned friend
        friends      = filter (/= playerAxial) friendAxials
    friends `shouldSatisfy` all (`elem` neigbours playerAxial)

 describe "Inventory" $ do
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
        item2 = MkShopItem 2 (ShopWeapon Sword)
        haul1 = MkHaul { haulItems = Set.singleton item1, haulNewMoney = 10 }
        haul2 = MkHaul { haulItems = Set.singleton item2, haulNewMoney = 5  }
        result = runEvt (ShopUpdates (MkBought haul2))
               $ runEvt (ShopUpdates (MkBought haul1)) initialState
    result ^. game_player_inventory . inventroy_item
      `shouldBe` Set.fromList [item1, item2]
