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
      part1 = length . filter (>0) . uncurry countArrangements,
      part2 = sum . uncurry countArrangements
    }

type Towel = String

countArrangements :: [Towel] -> [String] -> [Int]
countArrangements towels = map (arrangementsForDesign towels)

arrangementsForDesign :: [Towel] -> String -> Int
arrangementsForDesign towels pattern = head counts
  where
    counts = map arrangements [0 .. length pattern]
    arrangements n | n == length pattern = 1
    arrangements n = sum [counts !! (n + length towel) | towel <- towels, towel `isPrefixOf` drop n pattern]
