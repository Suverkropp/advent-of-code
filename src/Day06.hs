module Day06
  ( handleInput,
    part1,
    part2,
  )
where

import Data.Array (assocs, bounds, inRange, indices, (!), (//))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set, empty, insert, singleton, member)
import Decode (Grid, readGrid)

data Direction = North | East | South | West
  deriving (Eq, Ord)

type Pos = (Int, Int)

handleInput :: String -> (Pos, Grid Bool)
handleInput s = (pos, obstacles)
  where
    grid = readGrid s
    obstacles = fmap (== '#') grid
    pos = fst . fromJust . find ((== '^') . snd) . assocs $ grid

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

step :: Direction -> Pos -> Pos
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

part1 :: (Pos, Grid Bool) -> Int
part1 (pos, obs) = length $ walk obs (singleton pos) (pos, North)

walk :: Grid Bool -> Set Pos -> (Pos, Direction) -> Set Pos
walk obs visited (pos, dir)
  | not $ inBounds obs newPos = visited
  | obs ! newPos = walk obs visited (pos, turnRight dir)
  | otherwise = walk obs (insert newPos visited) (newPos, dir)
  where
    newPos = step dir pos

inBounds :: Grid a -> Pos -> Bool
inBounds grid = inRange $ bounds grid

part2 :: (Pos, Grid Bool) -> Int
part2 (pos, obs) = length [p | p <- indices obs, p /= pos, causesCycle p]
  where
    causesCycle p = detectCycle (obs // [(p, True)]) empty (pos, North)

detectCycle :: Grid Bool -> Set (Pos, Direction) -> (Pos, Direction) -> Bool
detectCycle obs visited (pos, dir)
  | not $ inBounds obs newPos = False
  | (newPos, dir) `member` visited = True
  | obs ! newPos = detectCycle obs (insert (pos, turnRight dir) visited) (pos, turnRight dir)
  | otherwise = detectCycle obs (insert (newPos, dir) visited) (newPos, dir)
  where
    newPos = step dir pos
