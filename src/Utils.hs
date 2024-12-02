module Utils
  ( getInput,
    runDay,
  )
where

import Data.Text (pack, strip, unpack)

getInput :: Int -> IO String
getInput day = unpack . strip . pack <$> readFile filepath
  where
    filepath = "inputs/day" ++ dayStr ++ ".txt"
    dayStr
      | day < 10 = "0" ++ show day
      | otherwise = show day

runDay :: (Show a) => Int -> (String -> a) -> IO String
runDay dayNum func = show . func <$> getInput dayNum
