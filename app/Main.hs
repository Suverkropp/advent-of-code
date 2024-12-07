module Main (main) where

import Y2015.Day02 (day2)
import AoC (runPart1, runPart2)

main :: IO ()
main = do
  output <- runPart2 day2
  putStrLn output
