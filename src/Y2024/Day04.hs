module Day04
  ( part1,
    handleInput,
    getDirections,
    part2,
  )
where

import Data.Array (Array, assocs, bounds, indices, listArray, (!))
import Text.Regex.Posix ((=~))

type Grid = Array (Int, Int) Char

handleInput :: String -> Grid
handleInput s = listArray ((0, 0), (x - 1, y - 1)) $ filter (/= '\n') s
  where
    y = length $ lines s
    x = length s `div` y

part1 :: Grid -> Int
part1 = searchLists . getDirections

getDirections :: Grid -> [[Char]]
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

part2 :: Grid -> Int
part2 grid = length . filter (isX grid) . filter (not . onEdge grid) . map fst . filter ((== 'A') . snd) . assocs $ grid

onEdge :: Grid -> (Int, Int) -> Bool
onEdge grid (x, y) = x == fst minIndex || x == fst maxIndex || y == snd minIndex || y == snd maxIndex
  where
    (minIndex, maxIndex) = bounds grid

isX :: Grid -> (Int, Int) -> Bool
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
