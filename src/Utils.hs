module Utils
  ( Grid,
    readGrid,
    Direction (..),
    toDirection,
    min3,
    step,
    Pos,
  )
where

import Data.Array (Array, array)

data Direction = North | East | South | West
  deriving (Eq, Ord)

type Pos = (Int, Int)

toDirection :: Char -> Direction
toDirection '^' = North
toDirection '>' = East
toDirection 'v' = South
toDirection '<' = West
toDirection _ = undefined

step :: Direction -> Pos -> Pos
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

type Grid a = Array (Int, Int) a

readGrid :: String -> Grid Char
readGrid s = array ((0, 0), (mx - 1, my - 1)) [((x, y), c) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l]
  where
    my = length $ lines s
    mx = length s `div` my

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = min (min a b) c
