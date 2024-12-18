{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Fuse concatMap/map" #-}

module Y2024.Day08 (day8) where

import AoC
import Data.Array (assocs, bounds, inRange)
import Data.Containers.ListUtils (nubOrd)
import Data.List.Extra (groupSortOn)
import Utils (Grid, Pos, readGrid)

day8 :: AoC (Grid Char) Int Int
day8 =
  AoC
    { year = 2024,
      day = 8,
      handleInput = readGrid,
      part1 = length . antinodes normalAntinodes,
      part2 = length . antinodes resonentAntinodes
    }

antinodes :: ((Pos, Pos) -> Pos -> Pos -> [Pos]) -> Grid Char -> [Pos]
antinodes f g =
  nubOrd
    . concatMap (concatMap (uncurry (f (bounds g))))
    . map (pairs . map fst)
    . groupSortOn snd
    . filter ((/= '.') . snd)
    . assocs
    $ g

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

normalAntinodes :: (Pos, Pos) -> Pos -> Pos -> [Pos]
normalAntinodes r a b = filter (inRange r) [add (diff a b) a, add (diff b a) b]

resonentAntinodes :: (Pos, Pos) -> Pos -> Pos -> [Pos]
resonentAntinodes r a b = from a b ++ from b a
  where
    from x y = takeWhile (inRange r) $ iterate (add (diff x y)) x

diff :: Pos -> Pos -> Pos
diff (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

add :: Pos -> Pos -> Pos
add (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)
