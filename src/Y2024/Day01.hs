module Y2024.Day01 (day1) where

import AoC (AoC (..))
import Data.List (sort)

day1 :: AoC ([Int], [Int]) Int Int
day1 =
  AoC
    { day = 1,
      year = 2024,
      part1 = uncurry distance,
      part2 = uncurry similarity,
      handleInput = getLists
    }

getLists :: String -> ([Int], [Int])
getLists input = unzip $ map getPair lns
  where
    getPair :: String -> (Int, Int)
    getPair str = (head . toNums $ words str, head . tail . toNums $ words str)
    toNums = map read
    lns = lines input

distance :: [Int] -> [Int] -> Int
distance xs ys = sum . map abs $ zipWith (-) (sort xs) (sort ys)

similarity :: [Int] -> [Int] -> Int
similarity lefts rights = sum $ map (\x -> x * count x rights) lefts

count :: Int -> [Int] -> Int
count x = length . filter (== x)
