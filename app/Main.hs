module Main (main) where

import Y2015.Day08 (day8, parseString)
import AoC

main :: IO ()
main = do
  output <- runPart2 day8
  putStrLn output
