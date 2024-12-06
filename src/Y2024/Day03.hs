module Day03
  ( part1,
    part2,
    handleInput,
  )
where

import Text.Regex.Posix (getAllTextMatches, (=~))

mulPattern :: String
mulPattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

handleInput :: String -> String
handleInput = id

getMatches :: String -> String -> [String]
getMatches pattern input = getAllTextMatches $ input =~ pattern

countMuls :: [String] -> Int
countMuls = foldr ((\(x, y) z -> x * y + z) . read . drop 3) 0

part1 :: String -> Int
part1 = countMuls . getMatches mulPattern

doDontPattern :: String
doDontPattern = mulPattern ++ "|do\\(\\)|don't\\(\\)"

part2 :: String -> Int
part2 = countMuls . dropDont . getMatches doDontPattern

dropDont :: [String] -> [String]
dropDont [] = []
dropDont ("don't()" : strs) = dropDont $ dropWhile (/= "do()") strs
dropDont ("do()" : strs) = dropDont strs
dropDont (s : strs) = s : dropDont strs

