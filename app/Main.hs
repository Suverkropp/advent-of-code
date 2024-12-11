module Main (main) where

import Y2015.Day10 (day10)
import AoC

main :: IO ()
main = do
  output <- runPart2 day10
  putStrLn output
