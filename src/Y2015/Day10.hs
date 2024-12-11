module Y2015.Day10 (day10) where

import AoC
import Data.Char (digitToInt)
import Data.List (group)
import Utils (applyNTimes)

day10 :: AoC [Int]
day10 =
  AoC
    { year = 2015,
      day = 10,
      handleInput = map digitToInt,
      part1 = length . applyNTimes 40 lookAndSay,
      part2 = length . applyNTimes 50 lookAndSay
    }

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap (\l -> [length l, head l]) . group
