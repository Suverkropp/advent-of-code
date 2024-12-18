module Y2015.Day14 (day14) where

import AoC
import Data.List.Extra (wordsBy)
import Utils (numChar)
import Data.List (transpose)

day14 :: AoC [Reindeer] Int Int
day14 =
  AoC
    { year = 2015,
      day = 14,
      handleInput = map (toTriple . map read . wordsBy (not . numChar)) . lines,
      part1 = maximum . map (distanceIn 2503),
      part2 = maximum . scores 2503
    }

toTriple :: [a] -> (a, a, a)
toTriple [speed, fly, rest] = (speed, fly, rest)
toTriple _ = error "not the right number of elements"

type Reindeer = (Int, Int, Int)

distanceIn :: Int -> Reindeer -> Int
distanceIn time (speed, fly, rest)
  | time >= fly = speed * fly + distanceIn (time - fly - rest) (speed, fly, rest)
  | time <= 0 = 0
  | otherwise = time * speed

winnerAfter :: Int -> [Reindeer] -> [Bool]
winnerAfter n rs = map (== maxDist) distances
  where
    maxDist = maximum distances
    distances = map (distanceIn n) rs

scores :: Int -> [Reindeer] -> [Int]
scores n rs = map sum $ transpose [map fromEnum $ winnerAfter i rs | i <- [1..n]]
