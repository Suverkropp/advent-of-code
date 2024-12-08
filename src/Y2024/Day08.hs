{-# LANGUAGE TupleSections #-}

module Y2024.Day08 (day8) where

import AoC
import Data.Array (assocs, bounds, inRange)
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (groupSortOn)
import Utils (Grid, Pos, readGrid)

day8 :: AoC (Grid Char)
day8 =
  AoC
    { year = 2024,
      day = 8,
      handleInput = readGrid,
      part1 = length . antinodes normalAntinodes,
      part2 = length . antinodes resonentAntinodes
    }

antinodes :: (Pos -> Pos -> [Pos]) -> Grid Char -> [Pos]
antinodes f g =
  nubOrd
    . concatMap ((concatMap (filter (inRange $ bounds g) . uncurry f) . pairs) . map fst)
    . groupSortOn snd
    . filter ((/= '.') . snd)
    . assocs
    $ g

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

normalAntinodes :: Pos -> Pos -> [Pos]
normalAntinodes a b = [add (diff a b) a, add (diff b a) b]
  where
    diff (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)
    add (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

resonentAntinodes :: Pos -> Pos -> [Pos]
resonentAntinodes = undefined
