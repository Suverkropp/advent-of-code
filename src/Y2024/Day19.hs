module Y2024.Day19 (day19) where

import AoC
import Data.Bifunctor (bimap)
import Data.List (isPrefixOf, uncons)
import Data.List.Extra (splitOn)
import Data.Maybe (fromJust)

day19 :: AoC ([Towel], [String]) Int Int
day19 =
  AoC
    { year = 2024,
      day = 19,
      handleInput = bimap (splitOn ", ") tail . fromJust . uncons . lines,
      part1 = length . filter id . uncurry mapSolvable,
      part2 = sum . uncurry countArrangements
    }

type Towel = String

mapSolvable :: [Towel] -> [String] -> [Bool]
mapSolvable towels = map isSolvable
  where
    isSolvable :: String -> Bool
    isSolvable [] = True
    isSolvable pattern = any isSolvable [drop (length towel) pattern | towel <- towels, towel `isPrefixOf` pattern]

countArrangements :: [Towel] -> [String] -> [Int]
countArrangements towels = map (arrangementsForDesign towels)

arrangementsForDesign :: [Towel] -> String -> Int
arrangementsForDesign _ [] = 1
arrangementsForDesign towels pattern = sum [arrangementsForDesign towels (drop (length towel) pattern) | towel <- towels, towel `isPrefixOf` pattern]
