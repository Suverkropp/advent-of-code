module Main (main) where

import Day02 (part2, handleInput)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 2 (part2 . handleInput)
  putStrLn output
