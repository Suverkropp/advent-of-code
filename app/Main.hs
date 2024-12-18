module Main (main) where

import AoC
import Y2015.Day19

main :: IO ()
main = do
  output <- runPart2 day19
  putStrLn output
