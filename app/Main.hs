module Main (main) where

import Day01 (part2)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 1 part2
  putStrLn output
