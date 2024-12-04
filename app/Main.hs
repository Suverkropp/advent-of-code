module Main (main) where

import Day04 (part2, handleInput, getDirections)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 4 (part2 . handleInput)
  putStrLn output
