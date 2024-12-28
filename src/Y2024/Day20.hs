{-# LANGUAGE TupleSections #-}

module Y2024.Day20 (day20) where

import AoC
import Control.Monad (join)
import Data.Array (bounds, inRange, range, (!), (//))
import Data.Array.IArray ((!?))
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Tuple.Extra ((&&&))
import Utils (Grid, Pos, allSteps, dist, findInGrid, readGrid)

day20 :: AoC (Grid Bool, (Pos, Pos)) Int Int
day20 =
  AoC
    { year = 2024,
      day = 20,
      handleInput = (fmap (== '#') &&& (findInGrid 'E' &&& findInGrid 'S')) . readGrid,
      part1 = length . uncurry (findCheats 2 100),
      part2 = length . uncurry (findCheats 20 100)
    }

type Cheat = (Pos, Pos)

findCheats :: Int -> Int -> Grid Bool -> (Pos, Pos) -> [Int]
findCheats cheatSize minDiff walls (end, start) = concatMap (filter (<= threshold) . mapMaybe (cheatQuality fromStart fromEnd) . cheats cheatSize) starts
  where
    threshold = fromJust (fromEnd ! start) - minDiff
    fromEnd = distanceFrom walls end
    fromStart = distanceFrom walls start
    starts = filter (any (<= threshold) . (fromStart !)) . range . bounds $ walls

cheatQuality :: Grid (Maybe Int) -> Grid (Maybe Int) -> Cheat -> Maybe Int
cheatQuality fromStart fromEnd cheat = do
  distFromEnd <- join $ fromEnd !? snd cheat
  distFromStart <- join $ fromStart !? fst cheat
  return $ distFromEnd + distFromStart + uncurry dist cheat

cheats :: Int -> Pos -> [Cheat]
cheats steps (x, y) = [((x, y), (x', y')) | x' <- [x - steps .. x + steps], y' <- [y - steps .. y + steps], dist (x, y) (x', y') <= steps]

distanceFrom :: Grid Bool -> Pos -> Grid (Maybe Int)
distanceFrom walls end = go [(0, end)] $ (Nothing <$ walls) // [(end, Just 0)]
  where
    gridBounds = bounds walls
    go :: [(Int, Pos)] -> Grid (Maybe Int) -> Grid (Maybe Int)
    go [] grid = grid
    go ((steps, pos) : rest) grid = go (rest ++ map (steps + 1,) newPositions) $ grid // map (,Just (steps + 1)) newPositions
      where
        newPositions = filter (\p -> inRange gridBounds p && isNothing (grid ! p) && not (walls ! p)) $ allSteps pos
