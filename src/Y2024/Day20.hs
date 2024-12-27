{-# LANGUAGE TupleSections #-}

module Y2024.Day20 (day20) where

import AoC
import Data.Array (bounds, elems, inRange, listArray, range, (!), (//))
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
findCheats cheatSize threshold walls (end, start) = concatMap (filter withinThreshold . mapMaybe (cheatQuality fromStart fromEnd) . cheats cheatSize validEnds) starts
  where
    withinThreshold x = x <= base - threshold
    justWithinThreshold (Just x) = withinThreshold x
    justWithinThreshold Nothing = False
    base = fromJust $ fromEnd ! start
    fromEnd = distanceFrom walls end
    fromStart = distanceFrom walls start
    starts = filter (justWithinThreshold . (fromStart !)) . range . bounds $ walls
    validEnds = listArray (bounds walls) $ zipWith (\wall d -> not wall && justWithinThreshold d) (elems walls) (elems fromEnd)

cheatQuality :: Grid (Maybe Int) -> Grid (Maybe Int) -> Cheat -> Maybe Int
cheatQuality fromStart fromEnd cheat = do
  distFromEnd <- fromEnd ! snd cheat
  distFromStart <- fromStart ! fst cheat
  return $ distFromEnd + distFromStart + uncurry dist cheat

cheats :: Int -> Grid Bool -> Pos -> [Cheat]
cheats steps validEnds start = [(start, end) | end <- posWithinRange, inGrid end, validEnds ! end]
  where
    (x, y) = start
    posWithinRange = [(x', y') | x' <- [x - steps .. x + steps], y' <- [y - steps .. y + steps], dist start (x', y') <= steps, dist start (x', y') >= 2]
    inGrid = inRange $ bounds validEnds

distanceFrom :: Grid Bool -> Pos -> Grid (Maybe Int)
distanceFrom walls end = go [(0, end)] $ (Nothing <$ walls) // [(end, Just 0)]
  where
    gridBounds = bounds walls
    go :: [(Int, Pos)] -> Grid (Maybe Int) -> Grid (Maybe Int)
    go [] grid = grid
    go ((steps, pos) : rest) grid = go (rest ++ map (steps + 1,) newPositions) $ grid // map (,Just (steps + 1)) newPositions
      where
        newPositions = filter (\p -> inRange gridBounds p && isNothing (grid ! p) && not (walls ! p)) $ allSteps pos
