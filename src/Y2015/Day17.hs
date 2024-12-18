module Y2015.Day17 (day17) where

import AoC

day17 :: AoC [Int]
day17 =
  AoC
    { year = 2015,
      day = 17,
      handleInput = map read . lines,
      part1 = countCombinations 150,
      part2 = countLeastCombinations 150
    }

countCombinations :: Int -> [Int] -> Int
countCombinations 0 _ = 1
countCombinations target _ | target < 0 = 0
countCombinations _ [] = 0
countCombinations target (cont : rest) =
  countCombinations target rest + countCombinations (target - cont) rest

countLeastCombinations :: Int -> [Int] -> Int
countLeastCombinations target list = head $ filter (> 0) [countCombinationsWithN target n list | n <- [1 ..]]

countCombinationsWithN :: Int -> Int -> [Int] -> Int
countCombinationsWithN 0 0 _ = 1
countCombinationsWithN _ _ [] = 0
countCombinationsWithN target n (cont : rest)
  | target < 0 || n < 0 = 0
  | otherwise = countCombinationsWithN target n rest + countCombinationsWithN (target - cont) (n - 1) rest
