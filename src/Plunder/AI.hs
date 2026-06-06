{-# LANGUAGE TemplateHaskell #-}

-- | Enemy decision logic.
--
-- Pure: 'decideEnemy' inspects the board and the enemy's current memory and
-- produces an 'EnemyAction'. State integration lives in "Plunder.State".
module Plunder.AI
  ( EnemyMind (..)
  , defMind
  , mind_lastSeen
  , EnemyAction (..)
  , sightRadius
  , decideEnemy
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.List           (sortOn)
import qualified Data.Map.Strict     as Map
import           Plunder.Combat      (unit_hp, Health)
import           Plunder.Grid
import           Plunder.Pathfinding (findPath)

-- | What an enemy remembers between turns.  Currently just the last position
-- a Player was spotted at; lets enemies pursue after the player breaks
-- line-of-sight.
--
-- Decision: store memory in a separate 'Map Axial EnemyMind' on 'GameState'
-- rather than extending the 'Enemy' 'TileContent' constructor.  The latter
-- would have forced every existing test and Level codec to know about the
-- new field; the map keeps the change contained to the AI subsystem.
newtype EnemyMind = MkEnemyMind
  { _mind_lastSeen :: Maybe Axial
  } deriving (Show, Eq)

defMind :: EnemyMind
defMind = MkEnemyMind { _mind_lastSeen = Nothing }

makeLenses ''EnemyMind

-- | What 'decideEnemy' tells the caller to do this turn.  Positional fields
-- keep all three constructors total (no partial selectors).
data EnemyAction
  = EAttack Axial Axial EnemyMind   -- ^ from, target, new mind
  | EStep   Axial Axial EnemyMind   -- ^ from, one-tile step, new mind
  | EIdle   Axial       EnemyMind   -- ^ from, new mind
  deriving (Show, Eq)

-- | How far an enemy can see (in hex distance).
sightRadius :: Int
sightRadius = 2

-- | All Player axials on the board, paired with their HP (used to prefer
-- weaker targets when several are equidistant).
playersOnBoard :: Grid -> [(Axial, Health)]
playersOnBoard grid =
  [ (ax, u ^. unit_hp)
  | (ax, tile) <- Map.toList grid
  , Just (Player u) <- [tile ^. tile_content]
  ]

-- | Closest Player within 'sightRadius'.  Ties broken by lowest HP, then by
-- 'Axial' order, so the decision is deterministic.
spotPlayer :: Grid -> Axial -> Maybe Axial
spotPlayer grid here =
  let inSight = [ (ax, hp, hexDistance here ax)
                | (ax, hp) <- playersOnBoard grid
                , hexDistance here ax <= sightRadius
                ]
  in case sortOn (\(ax, hp, d) -> (d, hp, ax)) inSight of
       []             -> Nothing
       ((ax, _, _):_) -> Just ax

-- | A Player on a tile directly adjacent to @here@, if any.  Same tie-breaking
-- as 'spotPlayer' so attack target is deterministic.
adjacentPlayer :: Grid -> Axial -> Maybe Axial
adjacentPlayer grid here =
  let candidates = [ (ax, hp)
                   | ax <- neigbours here
                   , Just tile <- [Map.lookup ax grid]
                   , Just (Player u) <- [tile ^. tile_content]
                   , let hp = u ^. unit_hp
                   ]
  in case sortOn (\(ax, hp) -> (hp, ax)) candidates of
       []         -> Nothing
       ((ax,_):_) -> Just ax

-- | Decide what one enemy does this turn.
--
-- Rules, in order:
--
--   1. Adjacent Player ⇒ 'EAttack' it (prefer lowest HP).
--   2. Player in sight ⇒ refresh memory, step one tile along BFS toward them.
--   3. Stale memory of a Player ⇒ step one tile toward the remembered tile.
--      The memory clears when the enemy reaches the spot, otherwise it
--      keeps walking toward where the player last was.
--   4. No sight, no memory ⇒ 'EIdle'.
decideEnemy :: Grid -> Axial -> EnemyMind -> EnemyAction
decideEnemy grid here oldMind =
  let spotted    = spotPlayer grid here
      remembered = spotted <|> _mind_lastSeen oldMind
      clearedMind = case remembered of
        Just t | t == here -> defMind
        _                  -> MkEnemyMind { _mind_lastSeen = remembered }
  in case adjacentPlayer grid here of
       Just target -> EAttack here target clearedMind
       Nothing -> case remembered of
         Nothing     -> EIdle here clearedMind
         Just target -> case findPath grid here target of
           Just (next:_) -> EStep here next clearedMind
           _             -> EIdle here clearedMind
