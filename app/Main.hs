module Main (main) where

import Y2024.Day11 (day11, lookupTable)
import AoC

main :: IO ()
main = do
  output <- runPart2 day11
  putStrLn output
