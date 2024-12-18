module Y2015.Day18 (day18) where

import AoC
import Data.Array (array, assocs, bounds, elems, inRange, (!), (//))
import Utils (Grid, Pos, applyNTimes, readGrid)

day18 :: AoC (Grid Bool)
day18 =
  AoC
    { year = 2015,
      day = 18,
      handleInput = fmap (== '#') . readGrid,
      part1 = length . filter id . elems . applyNTimes 100 gameOfLife,
      part2 = length . filter id . elems . applyNTimes 100 (turnOnCorners . gameOfLife) . turnOnCorners
    }

applyRules :: Int -> Bool -> Bool
applyRules n wasOn = n == 3 || (n == 2 && wasOn)

gameOfLife :: Grid Bool -> Grid Bool
gameOfLife grid = array (bounds grid) $ map cellStep $ assocs grid
  where
    cellStep :: (Pos, Bool) -> (Pos, Bool)
    cellStep (pos, wasOn) = (pos, applyRules (neighboursOn pos) wasOn)
    neighboursOn :: Pos -> Int
    neighboursOn = length . filter (grid !) . filter (inRange (bounds grid)) . neighbours

neighbours :: Pos -> [Pos]
neighbours (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

turnOnCorners :: Grid Bool -> Grid Bool
turnOnCorners grid = grid // [((0, 0), True), ((0, 99), True), ((99, 99), True), ((99, 0), True)]
