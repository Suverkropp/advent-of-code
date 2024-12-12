module Main (main) where

import Y2024.Day12
import AoC
import Utils

main :: IO ()
main = do
  output <- runPart2 day12
  putStrLn output
