module Day07
  ( handleInput,
    part1,
    part2,
  )
where

import Data.Tuple.Extra ((***))
import Prelude hiding ((||))

handleInput :: String -> [(Int, [Int])]
handleInput = map ((read *** map read . words . tail) . break (== ':')) . lines

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (uncurry checkValid)

checkValid :: Int -> [Int] -> Bool
checkValid testVal xs = testVal `elem` foldl (flip func) [head xs] (tail xs)
  where
    func :: Int -> [Int] -> [Int]
    func num = concatMap (\res -> [res + num, res * num]) . filter (<= testVal)

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (uncurry checkValidConcat)

checkValidConcat :: Int -> [Int] -> Bool
checkValidConcat testVal xs = testVal `elem` foldl (flip func) [head xs] (tail xs)
  where
    func :: Int -> [Int] -> [Int]
    func num = concatMap (\res -> [res + num, res * num, res || num]) . filter (<= testVal)

(||) :: Int -> Int -> Int
(||) x y = read $ show x ++ show y
