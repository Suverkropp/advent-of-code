module Y2015.Day04 (day4) where

import AoC
import Data.Hash.MD5 (md5s, Str (Str))
import Data.Maybe (fromJust)
import Data.List (find)

day4 :: AoC String
day4 =
  AoC
    { year = 2015,
      day = 4,
      handleInput = id,
      part1 = mine 5,
      part2 = mine 6
    }

mine :: Int -> String -> Int
mine z s = fromJust $ find ((>=z) . zeros . md5s . Str . (s++) . show) [1..]
  where
    zeros = length . takeWhile (=='0')
