module Main (main) where

import Y2015.Day07 (day7)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day7
  putStrLn output
