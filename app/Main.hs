module Main (main) where

import Y2024.Day10 (day10)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day10
  putStrLn output
