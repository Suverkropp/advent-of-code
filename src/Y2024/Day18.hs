{-# LANGUAGE TupleSections #-}

module Y2024.Day18 (day18) where

import AoC
import Data.Array (inRange, listArray, (!), (//))
import Data.Maybe (fromJust, isNothing)
import Utils (Grid, Pos, binarySearch, readPos)

day18 :: AoC [Pos] Int Pos
day18 =
  AoC
    { year = 2024,
      day = 18,
      handleInput = map readPos . lines,
      part1 = fromJust . findShortestPath . gridOf . take 1024,
      part2 = firstBlocking
    }

firstBlocking :: [Pos] -> Pos
firstBlocking poss = poss !! (binarySearch (\i -> isBlocked $ take i poss) 0 (length poss) - 1)
  where
    isBlocked = isNothing . findShortestPath . gridOf

start :: Pos
start = (0, 0)

end :: Pos
end = (70, 70)

gridOf :: [Pos] -> Grid Bool
gridOf poss = emptyGrid // map (,True) poss
  where
    emptyGrid = listArray (start, end) $ repeat False

findShortestPath :: Grid Bool -> Maybe Int
findShortestPath fallenBytes = go fallenBytes [(0, start)]
  where
    go :: Grid Bool -> [(Int, Pos)] -> Maybe Int
    go _ [] = Nothing
    go visited ((steps, pos) : rest)
      | pos == end = Just steps
      | otherwise = go (visited // map (,True) newPositions) $ rest ++ map (steps + 1,) newPositions
      where
        newPositions = filter (not . (visited !)) $ allSteps pos

allSteps :: Pos -> [Pos]
allSteps (x, y) = filter (inRange (start, end)) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
