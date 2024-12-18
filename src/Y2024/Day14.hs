{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2024.Day14 (day14, toGrid) where

import AoC
import Data.Array (array, range)
import Data.Bifunctor (bimap)
import Data.List (find, sort)
import Data.List.Extra (groupSort)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.Tuple.Extra (both)
import Text.Parsec (parse, string)
import Text.Parsec.String (Parser)
import Utils (Grid, Pos, add, mul, posParser)

day14 :: AoC [Robot] Int Int
day14 =
  AoC
    { year = 2024,
      day = 14,
      handleInput = map readRobot . lines,
      part1 = product . map length . toList . splitInQuadrants . map (posAfterNSteps 100),
      part2 = fst . fromJust . find (containsLine 10 . snd) . afterSteps 0
    }

type Robot = (Pos, Pos)

readRobot :: String -> Robot
readRobot s = case parse robotParser "" s of
  Left err -> error $ show err
  Right robot -> robot

robotParser :: Parser Robot
robotParser = do
  string "p="
  p <- posParser
  string " v="
  v <- posParser
  return (p, v)

area :: Pos
area = (101, 103)

middle :: Pos
middle = both (`div` 2) area

posAfterNSteps :: Int -> Robot -> Pos
posAfterNSteps n (p, v) = backToRange $ p `add` (n `mul` v)
  where
    backToRange (x, y) = bimap (myMod x) (myMod y) area
    myMod a b = let c = a `mod` b in if c < 0 then c + b else c

splitInQuadrants :: [Pos] -> ([Pos], [Pos], [Pos], [Pos])
splitInQuadrants = foldr func ([], [], [], [])
  where
    func (x, y) (q1, q2, q3, q4)
      | x < fst middle && y < snd middle = ((x, y) : q1, q2, q3, q4)
      | x > fst middle && y < snd middle = (q1, (x, y) : q2, q3, q4)
      | x < fst middle && y > snd middle = (q1, q2, (x, y) : q3, q4)
      | x > fst middle && y > snd middle = (q1, q2, q3, (x, y) : q4)
      | otherwise = (q1, q2, q3, q4)

toList :: ([Pos], [Pos], [Pos], [Pos]) -> [[Pos]]
toList (a, b, c, d) = [a, b, c, d]

afterSteps :: Int -> [Robot] -> [(Int, [Pos])]
afterSteps i rs = [(n, map (posAfterNSteps n) rs) | n <- [i ..]]

toGrid :: [Pos] -> Grid Char
toGrid ps = toChar <$> array gridRange [(p, p `elem` ps) | p <- range gridRange]
  where
    gridRange = ((0, 0), area `add` (-1, -1))
    toChar True = 'x'
    toChar False = ' '

containsLine :: Int -> [Pos] -> Bool
containsLine len ps = any ((>= len) . snd . foldl countLen (-10, 0)) rows
  where
    rows = map (sort . snd) . groupSort . map swap $ ps
    countLen (prev, num) a
      | a == prev + 1 = (a, num + 1)
      | num >= len = (prev, num)
      | otherwise = (a, 0)
