{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2015.Day09 (day9) where

import AoC
import Data.Containers.ListUtils (nubOrd)
import Data.List (permutations)
import Data.Map.Strict (fromList, (!), (!?))

day9 :: AoC [((String, String), Int)] Int Int
day9 =
  AoC
    { year = 2015,
      day = 9,
      handleInput = map parseLine . lines,
      part1 = minimum . visitAll,
      part2 = maximum . visitAll
    }

parseLine :: String -> ((String, String), Int)
parseLine s = let [from, _, to, _, dist] = words s in ((from, to), read dist)

distance :: (String -> String -> Int) -> [String] -> Int
distance dist (a : b : rest) = dist a b + distance dist (b : rest)
distance _ [_] = 0
distance _ [] = 0

getPlaces :: [((String, String), Int)] -> [String]
getPlaces = nubOrd . concatMap (\((x, y), _) -> [x, y])

visitAll :: [((String, String), Int)] -> [Int]
visitAll ls = map (distance dist) . permutations $ vars
  where
    distMap = fromList ls
    vars = getPlaces ls
    dist s1 s2 = case distMap !? (s1, s2) of
      Nothing -> distMap ! (s2, s1)
      Just v -> v
