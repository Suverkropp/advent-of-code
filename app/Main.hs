module Main (main) where

import Y2015.Day05 (day5)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day5
  putStrLn output
