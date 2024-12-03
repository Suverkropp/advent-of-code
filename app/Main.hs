module Main (main) where

import Day03 (part2, handleInput)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 3 (part2 . handleInput)
  putStrLn output
