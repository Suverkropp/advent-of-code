module Y2015.Day05 (day5) where

import AoC
import Data.List (isInfixOf)

day5 :: AoC [String]
day5 =
  AoC
    { year = 2015,
      day = 5,
      handleInput = lines,
      part1 = length . filter nice,
      part2 = length . filter newNice
    }

nice :: String -> Bool
nice s = vowels s && doubleLetter s && noForbidden s

vowels :: String -> Bool
vowels = (>=3) . length . filter (`elem` "aeiou")

doubleLetter :: String -> Bool
doubleLetter (a:b:_) | a==b = True
doubleLetter (_:s) = doubleLetter s
doubleLetter [] = False

noForbidden :: String -> Bool
noForbidden s = not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

newNice :: String -> Bool
newNice s = pairs s && sandwich s

pairs :: String -> Bool
pairs (a:b:s) = [a,b] `isInfixOf` s || pairs (b:s)
pairs _ = False

sandwich :: String -> Bool
sandwich (a:_:c:_) | a == c = True
sandwich (_:s) = sandwich s
sandwich [] = False
