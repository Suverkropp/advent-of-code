module Y2015.Day03 (day3) where

import AoC
import Utils (Direction (..), step, toDirection, Pos)
import Data.Containers.ListUtils (nubOrd)
import Data.Tuple.Extra (both)

day3 :: AoC [Direction] Int Int
day3 =
  AoC
    { year = 2015,
      day = 3,
      handleInput = map toDirection,
      part1 = length . visited,
      part2 = length . visitedWithRobo
    }

visited :: [Direction] -> [Pos]
visited = nubOrd . foldl (\ps d -> step d (head ps) : ps) [(0, 0)]

visitedWithRobo :: [Direction] -> [Pos]
visitedWithRobo = nubOrd . uncurry (++) . both visited . uninterleave

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs,ys) -> (ys,x:xs)) ([],[])

