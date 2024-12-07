module Main (main) where

import Y2024.Day01 (day1)
import AoC (runPart1)

main :: IO ()
main = do
  output <- runPart1 day1
  putStrLn output
