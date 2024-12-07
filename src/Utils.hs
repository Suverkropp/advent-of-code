module Utils
  ( Grid,
    readGrid,
    min3
  )
where

import Data.Array (Array, array)

type Grid a = Array (Int, Int) a

readGrid :: String -> Grid Char
readGrid s = array ((0, 0), (mx - 1, my - 1)) [((x, y), c) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l]
  where
    my = length $ lines s
    mx = length s `div` my

min3 :: Ord a => a -> a -> a -> a
min3 a b c = min (min a b) c
