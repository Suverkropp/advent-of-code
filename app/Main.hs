module Main (main) where

import Y2015.Day06 (day6)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day6
  putStrLn output
