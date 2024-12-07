module Y2024.Day04 (day4) where

import AoC
import Data.Array (assocs, bounds, indices, (!))
import Utils (Grid, readGrid)
import Text.Regex.Posix ((=~))

day4 :: AoC (Grid Char)
day4 =
  AoC
    { year = 2024,
      day = 4,
      handleInput = readGrid,
      part1 = countXMAS,
      part2 = countCross
    }

countXMAS :: Grid Char -> Int
countXMAS = searchLists . getDirections

getDirections :: Grid Char -> [[Char]]
getDirections grid = [[grid ! i | i <- indices grid, cons i j] | cons <- directions, j <- [-m .. m]]
  where
    m = uncurry (+) $ snd $ bounds grid
    directions = [horizontal, vertical, upDiagonal, downDiagonal]
    horizontal (x, _) j = x == j
    vertical (_, y) j = y == j
    downDiagonal (x, y) j = x + y == j
    upDiagonal (x, y) j = x - y == j

searchLists :: [[Char]] -> Int
searchLists = sum . map (\l -> (l =~ "XMAS") + (l =~ "SAMX"))

countCross :: Grid Char -> Int
countCross grid = length . filter (isX grid) . filter (not . onEdge grid) . map fst . filter ((== 'A') . snd) . assocs $ grid

onEdge :: Grid Char -> (Int, Int) -> Bool
onEdge grid (x, y) = x == fst minIndex || x == fst maxIndex || y == snd minIndex || y == snd maxIndex
  where
    (minIndex, maxIndex) = bounds grid

isX :: Grid Char -> (Int, Int) -> Bool
isX grid (x, y) = isMS a d && isMS b c
  where
    a = grid ! (x - 1, y - 1)
    b = grid ! (x - 1, y + 1)
    c = grid ! (x + 1, y - 1)
    d = grid ! (x + 1, y + 1)

isMS :: Char -> Char -> Bool
isMS 'M' 'S' = True
isMS 'S' 'M' = True
isMS _ _ = False
