module Test.StateSpec(spec) where

import           Control.Lens
import           Control.Monad.Trans.Random.Lazy (runRandT)
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import           Plunder.Combat (Weapon(..), isDead, unit_hp, unit_weapon, StatusEffect(..), unit_status, _DrinkingPotion, _Healing)
import           Plunder.Grid
import           Plunder.Shop
import           Plunder.State
import           System.Random                   (mkStdGen)
import           Test.Hspec
import           Test.QuickCheck                 ()

-- | Run a single update event against a game state (ignoring randomness)
runEvt :: UpdateEvts -> GameState -> GameState
runEvt evt gs = updateState initialState gs (rng, evt)
  where
    rng = MkRandTNT (\inner -> fst <$> runRandT inner (mkStdGen 42))

-- | The shop tile placed by the level function
shopAxial :: Axial
shopAxial = MkAxial 2 6

shopTileContent :: ShopContent
shopTileContent = MkShopContent (Just (MkShopItem 4 ShopHealthPotion)) (Just (MkShopItem 8 ShopUnit)) (Just (MkShopItem 5 (ShopWeapon Sword)))

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
        result = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul)) initialState
        players = result ^.. game_board . traversed . tile_content . _Just . _Player
    length players `shouldBe` 2

  it "buying a friend does not add it to the inventory" $ do
    let haul   = MkHaul { haulItems = Set.singleton (MkShopItem 0 ShopUnit), haulNewMoney = 0 }
        result = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul)) initialState
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
        result       = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul)) initialState
        playerAxial  = MkAxial 2 3
        friendAxials = result ^.. game_board
                                . itraversed
                                . filtered (has (tile_content . _Just . _Player))
                                . tile_coordinate
        -- remove the original player, leaving only the spawned friend
        friends      = filter (/= playerAxial) friendAxials
    friends `shouldSatisfy` all (`elem` neigbours playerAxial)

 describe "Inventory" $ do
  -- Regression: inventory clicks fire both UseItem and LeftClick in the same
  -- Reflex frame. leftmost must list UseItem first or LeftClick silently wins
  -- and items are never consumed.  This test pins the invariant: LeftClick
  -- must not touch the inventory.
  it "LeftClick does not consume inventory items" $ do
    let item       = MkShopItem 4 ShopHealthPotion
        withPotion = initialState
          & game_player_inventory . inventroy_item .~ Set.singleton item
    runEvt (LeftClick (MkAxial 2 3)) withPotion
      ^. game_player_inventory . inventroy_item `shouldBe` Set.singleton item

  it "UseItem with no selection falls back to first player on board" $ do
    let item  = MkShopItem 4 ShopHealthPotion
        noSel = initialState
          & game_player_inventory . inventroy_item .~ Set.singleton item
          & game_selected .~ Nothing
        result = runEvt (UseItem item) noSel
    -- item is consumed even without a selection
    result ^. game_player_inventory . inventroy_item `shouldBe` Set.empty
    -- and it was applied to the player
    result ^? game_board . ix (MkAxial 2 3) . tile_content . _Just . _Player . unit_status . _Just
      `shouldBe` Just DrinkingPotion

  it "UseItem with an enemy selected falls back to first player on board" $ do
    let item       = MkShopItem 5 (ShopWeapon Sword)
        enemyAxial = MkAxial 4 5
        withEnemy  = initialState
          & game_player_inventory . inventroy_item .~ Set.singleton item
          & game_selected .~ Just enemyAxial
        result = runEvt (UseItem item) withEnemy
    -- sword equipped; old axe swapped back into inventory
    result ^? game_board . ix (MkAxial 2 3) . tile_content . _Just . _Player . unit_weapon . _Just
      `shouldBe` Just Sword
    result ^. game_player_inventory . inventroy_item
      `shouldBe` Set.singleton (MkShopItem 0 (ShopWeapon Axe))

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
        result = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul)) initialState
    result ^. game_player_inventory . inventroy_item `shouldBe` Set.singleton item

  it "buying multiple items accumulates them" $ do
    let item1 = MkShopItem 4 ShopHealthPotion
        item2 = MkShopItem 2 (ShopWeapon Sword)
        haul1 = MkHaul { haulItems = Set.singleton item1, haulNewMoney = 10 }
        haul2 = MkHaul { haulItems = Set.singleton item2, haulNewMoney = 5  }
        -- Each purchase is committed separately with its own EndTurn
        result = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul2))
               $ runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul1)) initialState
    result ^. game_player_inventory . inventroy_item
      `shouldBe` Set.fromList [item1, item2]

 describe "Queued purchases" $ do
  let item = MkShopItem 4 ShopHealthPotion
      haul = MkHaul { haulItems = Set.singleton item, haulNewMoney = 5 }
      afterBuy = runEvt (ShopUpdates (MkBought haul)) initialState

  it "buying records a pending purchase" $
    afterBuy ^. game_pending_purchase `shouldBe` Just haul

  it "buying closes the shop" $
    afterBuy ^. game_shop `shouldBe` Nothing

  it "buying does not immediately add to inventory" $
    afterBuy ^. game_player_inventory . inventroy_item `shouldBe` Set.empty

  it "EndTurn applies the pending purchase to inventory" $
    runEvt EndTurn afterBuy ^. game_player_inventory . inventroy_item
      `shouldBe` Set.singleton item

  it "EndTurn clears the pending purchase" $
    runEvt EndTurn afterBuy ^. game_pending_purchase `shouldBe` Nothing

  it "planning a move cancels the pending purchase" $ do
    let playerAxial   = MkAxial 2 3
        destAxial     = MkAxial 2 4
        selectedAfterBuy = afterBuy & game_selected .~ Just playerAxial
        result = runEvt (RightClick destAxial) selectedAfterBuy
    result ^. game_pending_purchase `shouldBe` Nothing

  it "canceling a move does not cancel the pending purchase" $ do
    let playerAxial = MkAxial 2 3
        selectedAfterBuy = afterBuy & game_selected .~ Just playerAxial
        result = runEvt (RightClick playerAxial) selectedAfterBuy
    result ^. game_pending_purchase `shouldBe` Just haul

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

 describe "Terrain" $ do
  let playerAxial = MkAxial 2 3
      adjAxial    = MkAxial 2 4   -- adjacent empty tile in initial layout
      selectedState = initialState & game_selected .~ Just playerAxial

  it "Water tiles are not plannable" $ do
    let waterState = selectedState
          & game_board . ix adjAxial . tile_terrain .~ Water
        result = runEvt (RightClick adjAxial) waterState
    result ^. game_planned_moves `shouldBe` Map.empty

  it "Mountain tiles are not plannable" $ do
    let mtnState = selectedState
          & game_board . ix adjAxial . tile_terrain .~ Mountains
        result = runEvt (RightClick adjAxial) mtnState
    result ^. game_planned_moves `shouldBe` Map.empty

  it "Land tiles remain plannable" $ do
    let result = runEvt (RightClick adjAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.singleton playerAxial adjAxial

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

 describe "Queued attacks" $ do
  -- Player at MkAxial 2 3 has an Axe (from level).
  -- We place a weak enemy (1 HP) at MkAxial 2 4 (adjacent) so it always dies
  -- on EndTurn (Axe does Bigly = rng*2 ≥ 2 damage).
  let playerAxial  = MkAxial 2 3
      enemyAxial   = MkAxial 2 4  -- adjacent empty tile in initial layout
      houseAxial   = MkAxial 3 3  -- adjacent house in initial layout
      selectedState = initialState & game_selected .~ Just playerAxial
      weakEnemy    = Enemy (unit_hp .~ 1 $ defUnit)
      withEnemy    = selectedState
                       & game_board . ix enemyAxial . tile_content ?~ weakEnemy

  it "right-clicking an adjacent enemy does not kill it immediately" $ do
    let result = runEvt (RightClick enemyAxial) withEnemy
    result ^? game_board . ix enemyAxial . tile_content . _Just . _Enemy
      `shouldNotBe` Nothing

  it "right-clicking an adjacent enemy records a planned attack" $ do
    let result = runEvt (RightClick enemyAxial) withEnemy
    result ^. game_planned_moves `shouldBe` Map.singleton playerAxial enemyAxial

  it "EndTurn on a queued enemy attack kills a weak enemy" $ do
    -- Axe vs no-weapon: Bigly damage (rng*2 ≥ 2 > 1 HP) → always dies.
    let result = runEvt EndTurn
               $ runEvt (RightClick enemyAxial) withEnemy
    result ^? game_board . ix enemyAxial . tile_content . _Just . _Enemy
      `shouldBe` Nothing

  it "EndTurn on a queued enemy attack leaves blood at the target tile" $ do
    let result = runEvt EndTurn
               $ runEvt (RightClick enemyAxial) withEnemy
    result ^? game_board . ix enemyAxial . tile_background . _Just
      `shouldBe` Just Blood

  it "EndTurn on a queued attack clears planned_moves" $ do
    let result = runEvt EndTurn
               $ runEvt (RightClick enemyAxial) withEnemy
    result ^. game_planned_moves `shouldBe` Map.empty

  it "right-clicking an adjacent house does not destroy it immediately" $ do
    let result = runEvt (RightClick houseAxial) selectedState
    result ^? game_board . ix houseAxial . tile_content . _Just . _House
      `shouldNotBe` Nothing

  it "right-clicking an adjacent house records a planned attack" $ do
    let result = runEvt (RightClick houseAxial) selectedState
    result ^. game_planned_moves `shouldBe` Map.singleton playerAxial houseAxial

  it "EndTurn on a queued house attack destroys a weak house" $ do
    -- Replace the house with a weak one (1 HP) so it always dies.
    let weakHouseState = selectedState
          & game_board . ix houseAxial . tile_content ?~ House (unit_hp .~ 1 $ defUnit)
        result = runEvt EndTurn
               $ runEvt (RightClick houseAxial) weakHouseState
    result ^? game_board . ix houseAxial . tile_background . _Just
      `shouldBe` Just BurnedHouse

 describe "Selection" $ do
  it "LeftClick sets game_selected" $
    runEvt (LeftClick (MkAxial 2 3)) initialState ^. game_selected
      `shouldBe` Just (MkAxial 2 3)

  it "EndTurn clears game_selected" $ do
    let selected = initialState & game_selected .~ Just (MkAxial 2 3)
    runEvt EndTurn selected ^. game_selected `shouldBe` Nothing

 describe "Money" $ do
  let playerAxial = MkAxial 2 3
      houseAxial  = MkAxial 3 3
      selectedState = initialState & game_selected .~ Just playerAxial

  it "EndTurn applies haulNewMoney as the new wallet balance" $ do
    let haul   = MkHaul { haulItems = mempty, haulNewMoney = 42 }
        result = runEvt EndTurn $ runEvt (ShopUpdates (MkBought haul)) initialState
    result ^. game_player_inventory . inventory_money `shouldBe` 42

  it "destroying a weak house on EndTurn awards 10 money" $ do
    let weakHouseState = selectedState
          & game_board . ix houseAxial . tile_content ?~ House (unit_hp .~ 1 $ defUnit)
        result = runEvt EndTurn
               $ runEvt (RightClick houseAxial) weakHouseState
    result ^. game_player_inventory . inventory_money `shouldBe` 10

  it "right-clicking a shop does not create a planned move" $
    runEvt (RightClick shopAxial) playerAdjacentToShop ^. game_planned_moves
      `shouldBe` Map.empty

 describe "Combat" $ do
  it "isDead is True for 0 HP" $
    isDead 0 `shouldBe` True

  it "isDead is True for negative HP" $
    isDead (-1) `shouldBe` True

  it "isDead is False for positive HP" $
    isDead 1 `shouldBe` False

 describe "Health potion" $ do
  let item        = MkShopItem 4 ShopHealthPotion
      playerAxial = MkAxial 2 3
      withPotion  = initialState
        & game_player_inventory . inventroy_item .~ Set.singleton item
        & game_selected .~ Just playerAxial

  it "using a health potion removes it from inventory" $
    runEvt (UseItem item) withPotion
      ^. game_player_inventory . inventroy_item `shouldBe` Set.empty

  it "using a health potion marks the player as DrinkingPotion" $
    runEvt (UseItem item) withPotion
      ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_status . _Just
      `shouldBe` Just DrinkingPotion

  it "EndTurn after drinking applies first heal and transitions to Healing 4" $ do
    let result = runEvt EndTurn $ runEvt (UseItem item) withPotion
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_status . _Just
      `shouldBe` Just (Healing 4)
    -- player was already at maxHealth (10), healing is capped
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_hp
      `shouldBe` Just 10

  it "Healing ticks down and heals each EndTurn" $ do
    let healingState = initialState
          & game_board . ix playerAxial . tile_content . _Just . _Player . unit_hp .~ 5
          & game_board . ix playerAxial . tile_content . _Just . _Player . unit_status ?~ Healing 3
        result = runEvt EndTurn healingState
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_status . _Just
      `shouldBe` Just (Healing 2)
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_hp
      `shouldBe` Just 6

  it "Healing 0 is cleared on EndTurn without healing" $ do
    let healingState = initialState
          & game_board . ix playerAxial . tile_content . _Just . _Player . unit_status ?~ Healing 0
        result = runEvt EndTurn healingState
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_status
      `shouldBe` Just Nothing
    result ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_hp
      `shouldBe` Just 10

  it "health potion applies to the selected friend" $ do
    let potionItem  = MkShopItem 4 ShopHealthPotion
        friendAxial = MkAxial 1 3
        friendState = initialState
          & game_player_inventory . inventroy_item .~ Set.singleton potionItem
          & game_board . ix friendAxial . tile_content ?~ Player defUnit
          & game_selected .~ Just friendAxial
    runEvt (UseItem potionItem) friendState
      ^? game_board . ix friendAxial . tile_content . _Just . _Player . unit_status . _Just
      `shouldBe` Just DrinkingPotion

 describe "Weapon equip" $ do
  let weaponItem  = MkShopItem 5 (ShopWeapon Sword)
      playerAxial = MkAxial 2 3
      friendAxial = MkAxial 1 3
      -- player at playerAxial starts with Axe equipped (from level)
      withWeapon  = initialState
        & game_player_inventory . inventroy_item .~ Set.singleton weaponItem
        & game_selected .~ Just playerAxial

  it "using a weapon removes it from inventory" $
    runEvt (UseItem weaponItem) withWeapon
      ^. game_player_inventory . inventroy_item `shouldBe` Set.singleton (MkShopItem 0 (ShopWeapon Axe))

  it "using a weapon equips it on the selected player" $
    runEvt (UseItem weaponItem) withWeapon
      ^? game_board . ix playerAxial . tile_content . _Just . _Player . unit_weapon . _Just
      `shouldBe` Just Sword

  it "using a weapon swaps the previously equipped weapon back into inventory" $ do
    let result = runEvt (UseItem weaponItem) withWeapon
    result ^. game_player_inventory . inventroy_item
      `shouldBe` Set.singleton (MkShopItem 0 (ShopWeapon Axe))

  it "equipping a weapon on a friend with no weapon leaves inventory empty" $ do
    let friendState = withWeapon
          & game_board . ix friendAxial . tile_content ?~ Player defUnit
          & game_selected .~ Just friendAxial
        result = runEvt (UseItem weaponItem) friendState
    result ^? game_board . ix friendAxial . tile_content . _Just . _Player . unit_weapon . _Just
      `shouldBe` Just Sword
    result ^. game_player_inventory . inventroy_item `shouldBe` Set.empty

 describe "Fog of war" $ do
  -- Player starts at MkAxial 2 3 in the default level.
  let playerAxial = MkAxial 2 3

  it "the player's own tile is Visible" $
    tileVisibility initialState playerAxial `shouldBe` Visible

  it "a tile adjacent to the player is Visible" $
    tileVisibility initialState (MkAxial 2 4) `shouldBe` Visible

  it "a tile at distance 3 is Fog" $
    tileVisibility initialState (MkAxial 5 3) `shouldBe` Fog

  it "a tile at distance 5 that was never explored is Unexplored" $
    tileVisibility initialState (MkAxial 0 0) `shouldBe` Unexplored

  it "after moving closer then away, previously visible tile becomes Fog not Unexplored" $ do
    -- Move player to MkAxial 2 4, then back to MkAxial 2 3.
    -- MkAxial 2 6 is distance 2 from MkAxial 2 4 (visible), distance 3 from MkAxial 2 3 (fog).
    -- After moving away it should be Fog (explored) rather than Unexplored.
    let selected   = initialState & game_selected .~ Just playerAxial
        afterPlan  = runEvt (RightClick (MkAxial 2 4)) selected
        afterMove  = runEvt EndTurn afterPlan
        -- Now player is at MkAxial 2 4, move back
        selected2  = afterMove & game_selected .~ Just (MkAxial 2 4)
        afterPlan2 = runEvt (RightClick playerAxial) selected2
        afterBack  = runEvt EndTurn afterPlan2
    -- MkAxial 2 6 was within range when player was at 2 4, so it's now explored
    tileVisibility afterBack (MkAxial 2 6) `shouldBe` Fog

  it "explored set is initialized from player starting position" $
    Set.member playerAxial (initialState ^. game_explored) `shouldBe` True

  it "ResetGame re-initializes explored set" $ do
    let result = runEvt ResetGame (initialState & game_phase .~ YouDied)
    Set.member playerAxial (result ^. game_explored) `shouldBe` True
