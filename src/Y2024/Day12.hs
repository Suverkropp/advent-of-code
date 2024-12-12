module Y2024.Day12 (day12) where

import AoC
import Data.Array (assocs, bounds, elems, inRange, listArray, (!), (//))
import Data.Foldable (find)
import Data.List (group, sort)
import Data.List.Extra (groupSort)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Tuple.Extra (first)
import Utils (Direction (..), Grid, Pos, readGrid, step)

day12 :: AoC (Grid Char)
day12 =
  AoC
    { year = 2024,
      day = 12,
      handleInput = readGrid,
      part1 = fencePrice,
      part2 = discountPrice
    }

type Region = Int

type Edge = (Direction, Pos)

findRegionsAndPerimeters :: Grid Char -> ([[Edge]], Grid Region)
findRegionsAndPerimeters grid = first reverse $ go [] emptyGrid
  where
    emptyGrid :: Grid (Maybe Int)
    emptyGrid = listArray (bounds grid) (repeat Nothing)
    go :: [[Edge]] -> Grid (Maybe Int) -> ([[Edge]], Grid Int)
    go regionEdges regions = case fst <$> find (isNothing . snd) (assocs regions) of
      Nothing -> (regionEdges, fromJust <$> regions)
      Just p -> let (edges, newRegs) = fillInRegion (sameChars p) r (undefined, p) ([], regions) in go (edges : regionEdges) newRegs
        where
          r = length regionEdges
          sameChars p' = (== grid ! p') <$> grid

fillInRegion :: Grid Bool -> Region -> Edge -> ([Edge], Grid (Maybe Region)) -> ([Edge], Grid (Maybe Region))
fillInRegion grid r (d, p) (edges, regions)
  | not $ isInSameRegion grid p = ((d, p) : edges, regions)
  | isJust (regions ! p) = (edges, regions)
  | otherwise = foldr (fillInRegion grid r) (edges, newRegions) $ getEdges p
  where
    newRegions :: Grid (Maybe Region)
    newRegions = regions // [(p, Just r)]

getEdges :: Pos -> [Edge]
getEdges p = map (\d -> (d, step d p)) [North, East, South, West]

isInSameRegion :: Grid Bool -> Pos -> Bool
isInSameRegion grid p = inRange (bounds grid) p && grid ! p

regionAreas :: Grid Region -> [Int]
regionAreas = map length . group . sort . elems

fencePrice :: Grid Char -> Int
fencePrice grid = sum $ zipWith (*) areas perims
  where
    perims = map length edges
    (edges, regions) = findRegionsAndPerimeters grid
    areas = regionAreas regions

regionSides :: [Edge] -> Int
regionSides = sum . map (countRuns . sort) . pairsToLines . map toAlongAcross
  where
    isVert d = d == North || d == South
    toAlongAcross (d, (x, y)) = if isVert d then ((d, y), x) else ((d, x), y)
    pairsToLines = map snd . groupSort

countRuns :: [Int] -> Int
countRuns nums = fst $ foldl func (1, head nums) $ tail nums
  where
    func :: (Int, Int) -> Int -> (Int, Int)
    func (count, j) k = (if k == j + 1 then count else count + 1, k)

discountPrice :: Grid Char -> Int
discountPrice grid = sum $ zipWith (*) areas numSides
  where
    numSides = map regionSides edges
    (edges, regions) = findRegionsAndPerimeters grid
    areas = regionAreas regions
