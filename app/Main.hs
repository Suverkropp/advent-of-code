module Main (main) where

import Y2024.Day09 (day9)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day9
  putStrLn output
