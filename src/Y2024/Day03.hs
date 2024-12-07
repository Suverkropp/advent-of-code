module Y2024.Day03 (day3) where

import AoC (AoC (..))
import Text.Regex.Posix (getAllTextMatches, (=~))

day3 :: AoC String
day3 =
  AoC
    { year = 2024,
      day = 3,
      handleInput = id,
      part1 = addMuls,
      part2 = addDoMuls
    }

mulPattern :: String
mulPattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

getMatches :: String -> String -> [String]
getMatches pattern input = getAllTextMatches $ input =~ pattern

countMuls :: [String] -> Int
countMuls = foldr ((\(x, y) z -> x * y + z) . read . drop 3) 0

addMuls :: String -> Int
addMuls = countMuls . getMatches mulPattern

doDontPattern :: String
doDontPattern = mulPattern ++ "|do\\(\\)|don't\\(\\)"

addDoMuls :: String -> Int
addDoMuls = countMuls . dropDont . getMatches doDontPattern

dropDont :: [String] -> [String]
dropDont [] = []
dropDont ("don't()" : strs) = dropDont $ dropWhile (/= "do()") strs
dropDont ("do()" : strs) = dropDont strs
dropDont (s : strs) = s : dropDont strs
