module Main (main) where

import Y2024.Day11 (day11)
import AoC

main :: IO ()
main = do
  putStrLn "start"
  output <- runPart2 day11
  putStrLn output
