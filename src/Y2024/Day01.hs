module Day01
  ( day01,
    part2,
  )
where

import Data.List (sort)

day01 :: String -> Int
day01 input = distance xs ys
  where
    (xs, ys) = handleInput input

handleInput :: String -> ([Int], [Int])
handleInput input = unzip $ map getPair lns
  where
    getPair :: String -> (Int, Int)
    getPair str = (head . toNums $ words str, head . tail . toNums $ words str)
    toNums = map read
    lns = lines input

distance :: [Int] -> [Int] -> Int
distance xs ys = sum . map abs $ zipWith (-) (sort xs) (sort ys)

part2 :: String -> Int
part2 input = similarity xs ys
  where
    (xs, ys) = handleInput input

similarity :: [Int] -> [Int] -> Int
similarity lefts rights = sum $ map (\x -> x * count x rights) lefts

count :: Int -> [Int] -> Int
count x = length . filter (== x)
