module Y2024.Day11 (day11, lookupTable) where

import AoC
import Data.Map.Lazy (Map, fromList, (!))
import Utils (applyNTimes)

day11 :: AoC [Int] Int Int
day11 =
  AoC
    { year = 2024,
      day = 11,
      handleInput = map read . words,
      part1 = length . applyNTimes 25 blink,
      part2 = sum . map (blinkLength 75)
    }

blink :: [Int] -> [Int]
blink = concatMap blinkStone

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone n | even $ length digits = map read [take half digits, drop half digits]
  where
    digits = show n
    half = length digits `div` 2
blinkStone n = [2024 * n]

lookupTable :: Map (Int, Int) Int
lookupTable = fromList $ [((n, s), blinkLength n s) | n <- [0 .. 75], s <- [0 .. 9]]

blinkLength :: Int -> Int -> Int
blinkLength 0 _ = 1
blinkLength n s = sum $ map (blinkRest (n - 1)) $ blinkStone s
  where
    blinkRest n' s'
      | s' <= 9 = lookupTable ! (n', s')
      | otherwise = blinkLength n' s'
