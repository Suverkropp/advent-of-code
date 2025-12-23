module Main (main) where

import AoC
import Y2024.Day21

main :: IO ()
main = do
  -- print $ filter (\x -> length x <= 68) $ codeToMoves "029A"
  output <- runPart1 day21
  putStrLn output
