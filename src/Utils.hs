module Utils
  ( Grid,
    readGrid,
  )
where

import Data.Array (Array, array)

type Grid a = Array (Int, Int) a

readGrid :: String -> Grid Char
readGrid s = array ((0, 0), (mx - 1, my - 1)) [((x, y), c) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l]
  where
    my = length $ lines s
    mx = length s `div` my
