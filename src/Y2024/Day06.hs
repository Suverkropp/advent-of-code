module Y2024.Day06 (day6) where

import AoC
import Data.Array (assocs, bounds, inRange, (!), (//))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set, empty, insert, member, singleton, toList)
import Decode (Grid, readGrid)
import GHC.Utils.Misc (count)

data Direction = North | East | South | West
  deriving (Eq, Ord)

type Pos = (Int, Int)

day6 :: AoC (Pos, Grid Bool)
day6 =
  AoC
    { year = 2024,
      day = 6,
      handleInput = readGridAndPos,
      part1 = uncurry countVisited,
      part2 = uncurry countCycles
    }

readGridAndPos :: String -> (Pos, Grid Bool)
readGridAndPos s = (pos, obstacles)
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

countVisited :: Pos -> Grid Bool -> Int
countVisited pos obs = length $ walk obs (singleton pos) (pos, North)

walk :: Grid Bool -> Set Pos -> (Pos, Direction) -> Set Pos
walk obs visited (pos, dir)
  | not $ inBounds obs newPos = visited
  | obs ! newPos = walk obs visited (pos, turnRight dir)
  | otherwise = walk obs (insert newPos visited) (newPos, dir)
  where
    newPos = step dir pos

inBounds :: Grid a -> Pos -> Bool
inBounds grid = inRange $ bounds grid

countCycles :: Pos -> Grid Bool -> Int
countCycles pos obs = count (\p -> (p /= pos) && causesCycle p) $ toList $ walk obs (singleton pos) (pos, North)
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
