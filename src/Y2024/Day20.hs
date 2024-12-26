{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move mapMaybe" #-}

module Y2024.Day20 (day20) where

import AoC
import Data.Array (bounds, inRange, range, (!), (//))
import Data.Array.IArray ((!?))
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Tuple.Extra ((&&&))
import Utils (Grid, Pos, allSteps, findInGrid, readGrid)

day20 :: AoC (Maze, Pos) Int Int
day20 =
  AoC
    { year = 2024,
      day = 20,
      handleInput = ((fmap (== '#') &&& findInGrid 'E') &&& findInGrid 'S') . readGrid,
      part1 = length . uncurry (findCheats 100),
      part2 = undefined
    }

type Maze = (Grid Bool, Pos)

type Cheat = (Pos, Pos)

findCheats :: Int -> Maze -> Pos -> [Int]
findCheats threshold (walls, end) start = filter withinThreshold . mapMaybe cheatQuality . concatMap (cheats walls) . range . bounds $ walls
  where
    withinThreshold x = x <= base - threshold
    base = fromJust $ fromEnd ! start
    fromEnd = distanceFrom walls end
    fromStart = distanceFrom walls start
    cheatQuality :: Cheat -> Maybe Int
    cheatQuality cheat = do
      mDistFromEnd <- fromEnd !? snd cheat
      distFromEnd <- mDistFromEnd
      mDistFromStart <- fromStart !? fst cheat
      distFromStart <- mDistFromStart
      return $ distFromEnd + distFromStart + 2

cheats :: Grid Bool -> Pos -> [Cheat]
cheats walls start = map (start,) . filter notWallEnd . filter inGrid $ twoSteps
  where
    (x, y) = start
    twoSteps = [(x + 2, y), (x - 2, y), (x, y + 2), (x, y - 2), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]
    inGrid = inRange $ bounds walls
    notWallEnd = not . (walls !)

distanceFrom :: Grid Bool -> Pos -> Grid (Maybe Int)
distanceFrom walls end = go [(0, end)] $ (Nothing <$ walls) // [(end, Just 0)]
  where
    gridBounds = bounds walls
    go :: [(Int, Pos)] -> Grid (Maybe Int) -> Grid (Maybe Int)
    go [] grid = grid
    go ((steps, pos) : rest) grid = go (rest ++ map (steps + 1,) newPositions) $ grid // map (,Just (steps + 1)) newPositions
      where
        newPositions = filter (\p -> inRange gridBounds p && isNothing (grid ! p) && not (walls ! p)) $ allSteps pos
