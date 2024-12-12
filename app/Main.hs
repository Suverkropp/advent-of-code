module Main (main) where

import Y2024.Day12
import AoC
import Utils

main :: IO ()
main = do
  -- input <- readGrid <$> getInput 2024 12
  -- print $ findRegions input

  output <- runPart1 day12
  putStrLn output
