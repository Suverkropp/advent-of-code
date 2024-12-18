module AoC
  ( getInput,
    runPart1,
    runPart2,
    AoC (..),
  )
where

data (Show b, Show c) => AoC a b c = AoC
  { part1 :: a -> b,
    part2 :: a -> c,
    handleInput :: String -> a,
    day :: Int,
    year :: Int
  }

getInput :: Int -> Int -> IO String
getInput y d = readFile filepath
  where
    filepath = "inputs/" ++ show y ++ "/day" ++ dayStr ++ ".txt"
    dayStr
      | d < 10 = "0" ++ show d
      | otherwise = show d

runPart1 :: (Show b, Show c) => AoC a b c -> IO String
runPart1 aoc = show . part1 aoc . handleInput aoc <$> getInput (year aoc) (day aoc)

runPart2 :: (Show b, Show c) => AoC a b c -> IO String
runPart2 aoc = show . part2 aoc . handleInput aoc <$> getInput (year aoc) (day aoc)
