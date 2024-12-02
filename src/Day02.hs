module Day02
  ( part1,
    part2,
    handleInput,
  )
where

import GHC.Utils.Misc (count)

handleInput :: String -> [[Int]]
handleInput = map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 = count isSafe

part2 :: [[Int]] -> Int
part2 = count (any isSafe . dampen)

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
