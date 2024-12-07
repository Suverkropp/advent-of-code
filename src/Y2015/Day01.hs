module Y2015.Day01 (day1) where

import AoC

day1 :: AoC [Int]
day1 =
  AoC
    { year = 2015,
      day = 1,
      handleInput = map bracketNum,
      part1 = getFloor,
      part2 = getBasementPosition
    }

getFloor :: [Int] -> Int
getFloor = sum

bracketNum :: Char -> Int
bracketNum '(' = 1
bracketNum ')' = -1
bracketNum _ = 0

getBasementPosition :: [Int] -> Int
getBasementPosition = go 0 . zip [1 ..]
  where
    go _ [] = undefined
    go n ((i, c) : xs)
      | c + n == -1 = i
      | otherwise = go (c + n) xs
