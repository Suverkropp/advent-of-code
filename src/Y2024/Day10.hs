module Y2024.Day10 (day10) where

import AoC
import Data.Array (assocs, bounds, (!))
import Data.Char (digitToInt)
import Data.Ix (inRange)
import Utils (Grid, Pos, readGrid)
import Data.Containers.ListUtils (nubOrd)

day10 :: AoC (Grid Int) Int Int
day10 =
  AoC
    { year = 2024,
      day = 10,
      handleInput = fmap digitToInt . readGrid,
      part1 = sum . trailHeadScores,
      part2 = sum . trailHeadRatings
    }

trailHeadScores :: Grid Int -> [Int]
trailHeadScores grid = map (score grid) $ findTrailHeads grid

findTrailHeads :: Grid Int -> [Pos]
findTrailHeads = map fst . filter ((== 0) . snd) . assocs

score :: Grid Int -> Pos -> Int
score grid pos = length . nubOrd $ findEnds grid pos

findEnds :: Grid Int -> Pos -> [Pos]
findEnds grid pos
  | val == 9 = [pos]
  | otherwise = concatMap (findEnds grid) (filter stepIncreases $ getAllSteps grid pos)
  where
    val = grid ! pos
    stepIncreases p = grid ! p == val + 1

getAllSteps :: Grid Int -> Pos -> [Pos]
getAllSteps grid (x,y) = filter inBounds [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
  where
    inBounds = inRange (bounds grid)

trailHeadRatings :: Grid Int -> [Int]
trailHeadRatings grid = map (rating grid) $ findTrailHeads grid

rating :: Grid Int -> Pos -> Int
rating grid pos = length $ findEnds grid pos
