module Y2024.Day07 (day7) where

import AoC
import Data.Tuple.Extra ((***))
import Prelude hiding ((||))

day7 :: AoC [(Int, [Int])]
day7 =
  AoC
    { year = 2024,
      day = 7,
      handleInput = readEquations,
      part1 = addValids,
      part2 = addValidConcat
    }

readEquations :: String -> [(Int, [Int])]
readEquations = map ((read *** map read . words . tail) . break (== ':')) . lines

addValids :: [(Int, [Int])] -> Int
addValids = sum . map fst . filter (uncurry checkValid)

checkValid :: Int -> [Int] -> Bool
checkValid testVal xs = testVal `elem` foldl (flip func) [head xs] (tail xs)
  where
    func :: Int -> [Int] -> [Int]
    func num = concatMap (\res -> [res + num, res * num]) . filter (<= testVal)

addValidConcat :: [(Int, [Int])] -> Int
addValidConcat = sum . map fst . filter (uncurry checkValidConcat)

checkValidConcat :: Int -> [Int] -> Bool
checkValidConcat testVal xs = testVal `elem` foldl (flip func) [head xs] (tail xs)
  where
    func :: Int -> [Int] -> [Int]
    func num = concatMap (\res -> [res + num, res * num, res || num]) . filter (<= testVal)

(||) :: Int -> Int -> Int
(||) x y = read $ show x ++ show y
