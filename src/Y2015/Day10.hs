module Y2015.Day10 (day10) where

import AoC
import Data.Char (digitToInt)
import Data.List (group)

day10 :: AoC [Int]
day10 =
  AoC
    { year = 2015,
      day = 10,
      handleInput = map digitToInt,
      part1 = length . applyN 40 lookAndSay,
      part2 = length . applyN 50 lookAndSay
    }

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap (\l -> [length l, head l]) . group
