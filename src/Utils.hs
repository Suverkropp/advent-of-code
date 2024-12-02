module Utils
  ( getInput,
    runDay,
  )
where

getInput :: Int -> IO String
getInput day = readFile filepath
  where
    filepath = "inputs/day" ++ dayStr ++ ".txt"
    dayStr
      | day < 10 = "0" ++ show day
      | otherwise = show day

runDay :: (Show a) => Int -> (String -> a) -> IO String
runDay dayNum func = show . func <$> getInput dayNum
