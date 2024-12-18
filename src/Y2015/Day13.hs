{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2015.Day13 (day13) where

import AoC
import Data.List (nub, permutations)
import Data.Maybe (fromMaybe)

day13 :: AoC [((String, String), Int)] Int Int
day13 =
  AoC
    { year = 2015,
      day = 13,
      handleInput = map parseLine . lines,
      part1 = bestScore,
      part2 = bestScore . addSelf
    }

parseLine :: String -> ((String, String), Int)
parseLine str = let [n1, _, s, n, _, _, _, _, _, _, n2] = words str in ((n1, init n2), gainOrLose s * read n)
  where
    gainOrLose s = if s == "gain" then 1 else -1

score :: [((String, String), Int)] -> [String] -> Int
score prefs seating = score' $ seating ++ [head seating]
  where
    score' (a : b : rest) = pairScore a b + pairScore b a + score' (b : rest)
    score' [_] = 0
    score' [] = 0
    pairScore a b = fromMaybe 0 (lookup (a, b) prefs)

getNames :: [((String, String), Int)] -> [String]
getNames = nub . map (fst . fst)

bestScore :: [((String, String), Int)] -> Int
bestScore preferences = maximum [score preferences seating | seating <- permutations (getNames preferences)]

addSelf :: [((String, String), Int)] -> [((String, String), Int)]
addSelf prefs = (("self", "self"), 0) : prefs
