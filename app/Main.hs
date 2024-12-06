module Main (main) where

import Day06 (part2, handleInput)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 6 (part2 . handleInput)
  putStrLn output
