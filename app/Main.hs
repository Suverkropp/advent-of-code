module Main (main) where

import Y2024.Day11 (day11, showBlinkTrie)
import AoC

main :: IO ()
main = do
  putStrLn "start"
  output <- runPart1 day11
  putStrLn output
  output <- runPart2 day11
  putStrLn output
