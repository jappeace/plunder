-- | BFS pathfinding on the hex grid.
module Plunder.Pathfinding (findPath) where

import           Control.Lens     ((^.))
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Sequence    (Seq (..))
import qualified Data.Sequence    as Seq
import           Plunder.Grid

-- | Compute a shortest path from @src@ to @dst@ on the given grid.
--
--   * Intermediate tiles must be Land terrain with no content.
--   * The final tile must be Land terrain but may have content (enemy, house).
--   * Returns the path excluding @src@, including @dst@.
--   * Returns @Nothing@ when no valid path exists or @src == dst@.
findPath :: Grid -> Axial -> Axial -> Maybe [Axial]
findPath grid src dst
  | src == dst = Just []
  | otherwise  = bfs grid src dst (Seq.singleton src) (Set.singleton src) Map.empty

-- | All six hex neighbour offsets applied to a coordinate,
--   filtered to tiles that actually exist in the grid.
gridNeighbours :: Grid -> Axial -> [Axial]
gridNeighbours grid (MkAxial q r) =
  filter (`Map.member` grid)
    [ MkAxial (q + 1)  r
    , MkAxial  q      (r + 1)
    , MkAxial (q - 1) (r + 1)
    , MkAxial (q - 1)  r
    , MkAxial  q      (r - 1)
    , MkAxial (q + 1) (r - 1)
    ]

-- | Can we step onto this tile as an intermediate (not final) waypoint?
isPassable :: Grid -> Axial -> Bool
isPassable grid ax = case Map.lookup ax grid of
  Nothing   -> False
  Just tile -> case tile ^. tile_terrain of
    Land -> case tile ^. tile_content of
      Nothing -> True
      Just _  -> False
    Water     -> False
    Mountains -> False

-- | Can we step onto this tile as the final destination?
--   Must be Land terrain, but content is allowed (for attacks).
isFinalReachable :: Grid -> Axial -> Bool
isFinalReachable grid ax = case Map.lookup ax grid of
  Nothing   -> False
  Just tile -> case tile ^. tile_terrain of
    Land      -> True
    Water     -> False
    Mountains -> False

-- | BFS traversal: expand the frontier one level at a time, tracking
--   visited tiles and parent pointers for path reconstruction.
bfs :: Grid -> Axial -> Axial -> Seq Axial -> Set Axial -> Map Axial Axial -> Maybe [Axial]
bfs _ _ _ Empty _ _ = Nothing
bfs grid src dst (current :<| rest) visited parents
  | current == dst = Just (reconstructPath src parents dst)
  | otherwise =
      let nexts = [ n
                  | n <- gridNeighbours grid current
                  , not (Set.member n visited)
                  , if n == dst then isFinalReachable grid n else isPassable grid n
                  ]
          visited' = foldl (flip Set.insert) visited nexts
          parents' = foldl (\m n -> Map.insert n current m) parents nexts
          queue'   = rest <> Seq.fromList nexts
      in bfs grid src dst queue' visited' parents'

-- | Walk the parent map backwards from @node@ to @src@, building the path.
reconstructPath :: Axial -> Map Axial Axial -> Axial -> [Axial]
reconstructPath src parents = go []
  where
    go :: [Axial] -> Axial -> [Axial]
    go acc n
      | n == src  = acc
      | otherwise = case Map.lookup n parents of
          Nothing -> acc  -- should not happen
          Just p  -> go (n : acc) p
