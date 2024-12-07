module Main (main) where

import Y2024.Day07 (day7)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart1 day7
  putStrLn output
