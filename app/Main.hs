module Main (main) where

import Day02 (part1, handleInput)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 2 (part1 . handleInput)
  putStrLn output
