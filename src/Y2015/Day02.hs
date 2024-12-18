module Y2015.Day02 (day2) where

import AoC
import Data.List.Extra (wordsBy)
import Utils (min3)

day2 :: AoC [Dimensions] Int Int
day2 =
  AoC
    { year = 2015,
      day = 2,
      handleInput = map readDimensions . lines,
      part1 = sum . map requiredPaper,
      part2 = sum . map requiredRibbon
    }

type Dimensions = (Int, Int, Int)

readDimensions :: String -> Dimensions
readDimensions = take3 . map (read :: String -> Int) . wordsBy (== 'x')
  where
    take3 (x : y : z : _) = (x, y, z)
    take3 _ = undefined

requiredPaper :: Dimensions -> Int
requiredPaper (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l + smallestSideArea (l, w, h)

smallestSideArea :: Dimensions -> Int
smallestSideArea (l, w, h) = min3 (l * w) (l * h) (w * h)

requiredRibbon :: Dimensions -> Int
requiredRibbon d = smallestPerimeter d + volume d

smallestPerimeter :: Dimensions -> Int
smallestPerimeter (l, w, h) = (2 *) $ min3 (l + w) (l + h) (w + h)

volume :: Dimensions -> Int
volume (l, w, h) = l * w * h
