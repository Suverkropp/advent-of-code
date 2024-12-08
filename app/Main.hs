module Main (main) where

import Y2024.Day08 (day8)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart1 day8
  putStrLn output
