module Main (main) where

import Y2015.Day01 (day1)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day1
  putStrLn output
