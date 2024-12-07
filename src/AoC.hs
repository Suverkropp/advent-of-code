module AoC
  ( getInput,
    runPart1,
    runPart2,
    AoC (..),
  )
where

import Data.Text (pack, strip, unpack)

data AoC a = AoC
  { part1 :: a -> Int,
    part2 :: a -> Int,
    handleInput :: String -> a,
    day :: Int,
    year :: Int
  }

getInput :: Int -> Int -> IO String
getInput y d = unpack . strip . pack <$> readFile filepath
  where
    filepath = "inputs/" ++ show y ++ "/day" ++ dayStr ++ ".txt"
    dayStr
      | d < 10 = "0" ++ show d
      | otherwise = show d

runPart1 :: AoC a -> IO String
runPart1 aoc = show . part1 aoc . handleInput aoc <$> getInput (year aoc) (day aoc)

runPart2 :: AoC a -> IO String
runPart2 aoc = show . part2 aoc . handleInput aoc <$> getInput (year aoc) (day aoc)
