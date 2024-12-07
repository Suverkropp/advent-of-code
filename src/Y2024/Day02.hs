module Y2024.Day02
  ( day2
  )
where

import GHC.Utils.Misc (count)
import AoC (AoC(..))

day2 :: AoC [[Int]]
day2 = AoC {
    year = 2024,
    day = 2,
    part1 = countSafe,
    part2 = countDampenedSafe,
    handleInput = readLists
}

readLists :: String -> [[Int]]
readLists = map (map read . words) . lines

countSafe :: [[Int]] -> Int
countSafe = count isSafe

countDampenedSafe :: [[Int]] -> Int
countDampenedSafe = count (any isSafe . dampen)

data IncDec = Inc | Dec | Unknow deriving Eq

isSafe :: [Int] -> Bool
isSafe = go Unknow
 where
 go _ [] = True
 go _ [_] = True
 go incdec (x:y:xs) 
    | x < y && y - x < 4 && incdec /= Dec = go Inc (y:xs)
    | x > y && x - y < 4 && incdec /= Inc = go Dec (y:xs)
    | otherwise  = False

dampen :: [Int] -> [[Int]]
dampen = helper []
  where
    helper _ [] = []
    helper xs (y : ys) = (xs ++ ys) : helper (xs ++ [y]) ys
