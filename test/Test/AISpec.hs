module Test.AISpec (spec) where

import           Control.Lens
import           Control.Monad.Trans.Random.Lazy (runRandT)
import qualified Data.Map.Strict                 as Map
import           Plunder.AI                      (EnemyAction(..), EnemyMind(..), decideEnemy, defMind, mind_lastSeen, sightRadius)
import           Plunder.Combat                  (Weapon(..), Unit, defUnit, unit_hp, unit_weapon)
import           Plunder.Grid
import           Plunder.State
import           System.Random                   (mkStdGen)
import           Test.Hspec

-- | Run an event with a fixed RNG so combat damage is reproducible.
runEvt :: UpdateEvts -> GameState -> GameState
runEvt evt gs = updateState gs (rng, evt)
  where rng = MkRandTNT (\inner -> fst <$> runRandT inner (mkStdGen 7))

-- | A board with the standard 0..6 grid and nothing on it.
emptyState :: GameState
emptyState = initialState
  & game_board %~ fmap clearTile
  & game_enemy_minds .~ Map.empty
  where
    clearTile :: Tile -> Tile
    clearTile t = t & tile_content .~ Nothing
                    & tile_background .~ Nothing

-- | An unarmed attacker deals no damage (see 'Plunder.Combat.getDmg').
-- These tests want to observe combat outcomes, so units carry a weapon.
armedUnit :: Unit
armedUnit = defUnit & unit_weapon ?~ Sword

-- | Place an armed Player at @ax@.
withPlayer :: Axial -> GameState -> GameState
withPlayer ax gs = gs & game_board . ix ax . tile_content ?~ Player armedUnit

-- | Place an armed Enemy at @ax@.
withEnemy :: Axial -> GameState -> GameState
withEnemy ax gs = gs & game_board . ix ax . tile_content ?~ Enemy armedUnit

-- | The single Player's HP, if there is one.
firstPlayerHp :: GameState -> Maybe Int
firstPlayerHp gs = gs ^? game_board . traversed
                       . tile_content . _Just . _Player . unit_hp

-- | Whether there is an Enemy on this tile.
enemyAt :: Axial -> GameState -> Bool
enemyAt ax gs = has (game_board . ix ax . tile_content . _Just . _Enemy) gs

-- | All axials currently occupied by an Enemy.
enemyPositions :: GameState -> [Axial]
enemyPositions gs =
  [ ax | (ax, t) <- Map.toList (gs ^. game_board)
       , has (tile_content . _Just . _Enemy) t ]

spec :: Spec
spec = do
 describe "decideEnemy (pure)" $ do
  it "attacks an adjacent player" $ do
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 3 3)
    case decideEnemy (gs ^. game_board) (MkAxial 3 3) defMind of
      EAttack _ to _ -> to `shouldBe` MkAxial 2 3
      other -> expectationFailure $ "expected EAttack, got " <> show other

  it "steps toward a player in sight (distance 2)" $ do
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 4 3)   -- distance 2 from player
    case decideEnemy (gs ^. game_board) (MkAxial 4 3) defMind of
      EStep _ next mind -> do
        hexDistance next (MkAxial 2 3) `shouldBe` 1
        mind ^. mind_lastSeen `shouldBe` Just (MkAxial 2 3)
      other -> expectationFailure $ "expected EStep, got " <> show other

  it "is idle when no player is in sight and no memory" $ do
    let gs = emptyState
           & withPlayer (MkAxial 0 0)
           & withEnemy  (MkAxial 6 6)   -- distance > sightRadius
    case decideEnemy (gs ^. game_board) (MkAxial 6 6) defMind of
      EIdle _ mind -> mind ^. mind_lastSeen `shouldBe` Nothing
      other -> expectationFailure $ "expected EIdle, got " <> show other

  it "pursues a remembered tile after losing sight" $ do
    let gs = emptyState
           & withPlayer (MkAxial 0 0)
           & withEnemy  (MkAxial 6 6)
        mind = MkEnemyMind { _mind_lastSeen = Just (MkAxial 5 5) }
    case decideEnemy (gs ^. game_board) (MkAxial 6 6) mind of
      EStep _ next _ -> hexDistance next (MkAxial 5 5) `shouldBe` 1
      other -> expectationFailure $ "expected EStep toward remembered tile, got " <> show other

  it "clears memory once the remembered tile is reached" $ do
    let gs = emptyState
           & withPlayer (MkAxial 0 0)
           & withEnemy  (MkAxial 6 6)
        mind = MkEnemyMind { _mind_lastSeen = Just (MkAxial 6 6) }
    case decideEnemy (gs ^. game_board) (MkAxial 6 6) mind of
      EIdle _ newMind -> newMind ^. mind_lastSeen `shouldBe` Nothing
      other -> expectationFailure $ "expected EIdle after reaching last-seen tile, got " <> show other

  it "sightRadius is 2" $
    -- A change here would silently change AI behaviour; pin it.
    sightRadius `shouldBe` 2

 describe "runEnemies via EndTurn" $ do
  it "adjacent enemy hits the player on EndTurn" $ do
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 3 3)
        afterTurn = runEvt EndTurn gs
    firstPlayerHp afterTurn `shouldSatisfy` maybe False (< 10)

  it "enemy 2 tiles away moves one step closer" $ do
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 4 3)
        afterTurn = runEvt EndTurn gs
    enemyAt (MkAxial 4 3) afterTurn `shouldBe` False
    case enemyPositions afterTurn of
      [newPos] -> hexDistance newPos (MkAxial 2 3) `shouldBe` 1
      other    -> expectationFailure $ "expected one enemy on board, got " <> show other

  it "enemy out of sight and with no memory stays put" $ do
    let enemyAxial = MkAxial 6 6
        gs = emptyState
           & withPlayer (MkAxial 0 0)
           & withEnemy  enemyAxial
        afterTurn = runEvt EndTurn gs
    enemyAt enemyAxial afterTurn `shouldBe` True

  it "enemy records lastSeen after stepping" $ do
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 4 3)
        afterTurn = runEvt EndTurn gs
    case enemyPositions afterTurn of
      [newPos] ->
        afterTurn ^? game_enemy_minds . ix newPos . mind_lastSeen
          `shouldBe` Just (Just (MkAxial 2 3))
      other -> expectationFailure $ "expected one enemy on board, got " <> show other

  it "two enemies don't both land on the same tile" $ do
    -- Both enemies sit at distance 2 from the player and would naively step
    -- onto the same tile.  The second one must re-path or stay put.
    let gs = emptyState
           & withPlayer (MkAxial 2 3)
           & withEnemy  (MkAxial 4 3)
           & withEnemy  (MkAxial 4 2)
        afterTurn = runEvt EndTurn gs
        positions = enemyPositions afterTurn
    length positions `shouldBe` 2
    length (Map.keys (Map.fromList (zip positions positions))) `shouldBe` 2

  it "player killed by enemies triggers YouDied" $ do
    -- Pre-wound the player to 1 HP and surround so combat finishes them.
    let woundedPlayer = armedUnit & unit_hp .~ 1
        gs = emptyState
           & game_board . ix (MkAxial 2 3) . tile_content ?~ Player woundedPlayer
           & withEnemy (MkAxial 3 3)
           & withEnemy (MkAxial 2 4)
           & withEnemy (MkAxial 3 2)
        afterTurn = runEvt EndTurn gs
    afterTurn ^. game_phase `shouldBe` YouDied

  it "an enemy killed during the player phase doesn't act" $ do
    -- Player attacks a weak enemy on EndTurn; that enemy must not also swing.
    let weakEnemy = Enemy (defUnit & unit_hp .~ 1)
        gs = emptyState
           & withPlayer (MkAxial 2 3)
           & game_board . ix (MkAxial 3 3) . tile_content ?~ weakEnemy
           & game_selected .~ Just (MkAxial 2 3)
        planned   = runEvt (RightClick (MkAxial 3 3)) gs
        afterTurn = runEvt EndTurn planned
    -- The dead enemy is gone (player walked onto its tile), so no mind entry remains.
    afterTurn ^. game_enemy_minds . at (MkAxial 3 3) `shouldBe` Nothing
