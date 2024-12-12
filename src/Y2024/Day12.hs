module Y2024.Day12 (day12, findRegions) where

import AoC
import Data.Array (assocs, bounds, elems, inRange, listArray, (!), (//))
import Data.Foldable (find)
import Data.List (group, sort)
import Data.Maybe (fromJust, isJust, isNothing)
import Utils (Grid, Pos, readGrid)
import Data.Ord (Down(Down))

day12 :: AoC (Grid Char)
day12 =
  AoC
    { year = 2024,
      day = 12,
      handleInput = readGrid,
      part1 = fencePrice,
      part2 = undefined
    }

type Region = Int

findRegions :: Grid Char -> ([Int], Grid Region)
findRegions grid = go [] emptyGrid
  where
    emptyGrid :: Grid (Maybe Int)
    emptyGrid = listArray (bounds grid) (repeat Nothing)
    go :: [Int] -> Grid (Maybe Int) -> ([Int], Grid Int)
    go perims regions = case fst <$> find (isNothing . snd) (assocs regions) of
      Nothing -> (perims, fromJust <$> regions)
      Just p -> let (perim, newRegs) = fillInRegion grid r (grid ! p) p 0 regions in go (perim : perims) newRegs
        where
          r = length perims

fillInRegion :: Grid Char -> Region -> Char -> Pos -> Int -> Grid (Maybe Region) -> (Int, Grid (Maybe Region))
fillInRegion grid r c p i regions
  | not $ isInSameRegion grid c p = (i + 1, regions)
  | isJust (regions ! p) = (i, regions)
  | otherwise = foldr (\p' (i', regs) -> fillInRegion grid r c p' i' regs) (i, newRegions) $ neighbourCells p
  where
    newRegions :: Grid (Maybe Region)
    newRegions = regions // [(p, Just r)]

neighbourCells :: Pos -> [Pos]
neighbourCells (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isInSameRegion :: Grid Char -> Char -> Pos -> Bool
isInSameRegion grid c p = inRange (bounds grid) p && c == grid ! p

regionAreas :: Grid Region -> [Int]
regionAreas = map length . group . sort . map Down . elems

fencePrice :: Grid Char -> Int
fencePrice grid = sum $ zipWith (*) areas perims
  where
    (perims, regions) = findRegions grid
    areas = regionAreas regions
