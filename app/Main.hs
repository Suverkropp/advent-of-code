module Main (main) where

import Day07 (part2, handleInput)
import Utils (runDay)

main :: IO ()
main = do
  output <- runDay 7 (part2 . handleInput)
  putStrLn output
