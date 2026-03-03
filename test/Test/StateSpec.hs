module Test.StateSpec(spec) where

import           Control.Lens
import           Control.Monad.Trans.Random.Lazy (runRandT)
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import           Plunder.Combat (Weapon(..), unit_hp)
import           Plunder.Grid
import           Plunder.Shop
import           Plunder.State
import           System.Random                   (mkStdGen)
import           Test.Hspec
import           Test.QuickCheck                 ()

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

 describe "Death and lose condition" $ do
  -- Place a second Player with 0 HP adjacent to the original player
  let friendAxial    = MkAxial 1 3
      withDeadFriend = initialState
        & game_board . ix friendAxial . tile_content ?~ Player (unit_hp .~ 0 $ defUnit)

  it "dead friend is removed from the board" $ do
    let result = runEvt Redraw withDeadFriend
    result ^.. game_board . traversed . tile_content . _Just . _Player
      `shouldSatisfy` \ps -> length ps == 1

  it "blood is left at the dead friend's position" $ do
    let result = runEvt Redraw withDeadFriend
    result ^? game_board . ix friendAxial . tile_background . _Just
      `shouldBe` Just Blood

  it "game continues when friend dies but original player survives" $ do
    let result = runEvt Redraw withDeadFriend
    result ^? game_board . ix (MkAxial 2 3) . tile_content . _Just . _Player
      `shouldNotBe` Nothing

  it "phase becomes YouDied when all players are dead" $ do
    let allDeadState = initialState
          & game_board . ix (MkAxial 2 3) . tile_content ?~ Player (unit_hp .~ 0 $ defUnit)
        result = runEvt Redraw allDeadState
    result ^. game_phase `shouldBe` YouDied

  it "phase becomes YouVictorious when all enemies are gone" $ do
    let noEnemiesState = initialState
          & game_board . traversed . tile_content %~ \case
              Just (Enemy _) -> Nothing
              x              -> x
        result = runEvt Redraw noEnemiesState
    result ^. game_phase `shouldBe` YouVictorious

  it "ResetGame restores Playing phase and initial board" $ do
    let result = runEvt ResetGame (initialState & game_phase .~ YouDied)
    result ^. game_phase `shouldBe` Playing
    result ^? game_board . ix (MkAxial 2 3) . tile_content . _Just . _Player . unit_hp
      `shouldBe` Just 10

 describe "Turn-based movement" $ do
  -- Player starts at MkAxial 2 3; MkAxial 2 4 is an adjacent empty tile.
  let playerAxial = MkAxial 2 3
      destAxial   = MkAxial 2 4
      selectedState = initialState & game_selected .~ Just playerAxial

  it "right-clicking an adjacent empty tile no longer moves the unit immediately" $ do
    let result = runEvt (RightClick destAxial) selectedState
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player
      `shouldNotBe` Nothing

  it "right-clicking an adjacent empty tile records a planned move" $ do
    let result = runEvt (RightClick destAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.singleton playerAxial destAxial

  it "right-clicking a different adjacent tile overwrites the plan" $ do
    let altDest = MkAxial 1 3
        result  = runEvt (RightClick altDest)
                $ runEvt (RightClick destAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.singleton playerAxial altDest

  it "right-clicking the unit's own tile cancels the plan" $ do
    let result = runEvt (RightClick playerAxial)
               $ runEvt (RightClick destAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.empty

  it "EndTurn executes the planned move" $ do
    let result = runEvt EndTurn
               $ runEvt (RightClick destAxial) selectedState
    result ^? game_board . ix destAxial . tile_content . _Just . _Player
      `shouldNotBe` Nothing

  it "EndTurn clears the unit from its original tile" $ do
    let result = runEvt EndTurn
               $ runEvt (RightClick destAxial) selectedState
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player
      `shouldBe` Nothing

  it "EndTurn clears planned_moves" $ do
    let result = runEvt EndTurn
               $ runEvt (RightClick destAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.empty

  it "EndTurn skips a plan whose destination was taken by an earlier move" $ do
    -- Two players both plan to move to the same tile; only the first succeeds.
    let player2Axial = MkAxial 1 3  -- adjacent to destAxial
        stateTwo = selectedState
          & game_board . ix player2Axial . tile_content ?~ Player defUnit
          & game_planned_moves .~ Map.fromList
              [ (playerAxial, destAxial)
              , (player2Axial, destAxial)
              ]
        result = runEvt EndTurn stateTwo
        players = result ^.. game_board . traversed . tile_content . _Just . _Player
    length players `shouldBe` 2
