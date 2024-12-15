module Y2024.Day15 (day15) where

import AoC
import Data.Array (assocs, (!), (//))
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Utils (Direction (..), Grid, Pos, readGrid, step, toDirection, showGrid)
import Debug.Trace

day15 :: AoC (Warehouse, [Direction])
day15 =
  AoC
    { year = 2024,
      day = 15,
      handleInput = bimap (getRobot . readGrid) (map toDirection) . splitOnNewline,
      part1 = sum . map getGPS . getBoxes . traceWith showGrid . fst. uncurry (foldl applyMove),
      part2 = undefined
    }

type Warehouse = (Grid Char, Pos)

splitOnNewline :: String -> (String, String)
splitOnNewline = bimap (init . unlines) concat . break null . lines

getRobot :: Grid Char -> (Grid Char, Pos)
getRobot grid = let pos = fst $ fromJust $ find ((== '@') . snd) $ assocs grid in (grid // [(pos, '.')], pos)

getGPS :: Pos -> Int
getGPS (x, y) = x + 100 * y

getBoxes :: Grid Char -> [Pos]
getBoxes = map fst . filter ((== 'O') . snd) . assocs

applyMove :: Warehouse -> Direction -> Warehouse
applyMove (grid, pos) dir
  | grid ! newPos == '.' = (grid, newPos)
  | grid ! newPos == 'O' && isJust pushed = (fromJust pushed, newPos)
  | otherwise = (grid, pos)
  where
    newPos = step dir pos
    pushed = pushBox grid newPos dir

pushBox :: Grid Char -> Pos -> Direction -> Maybe (Grid Char)
pushBox grid pos dir 
  | grid ! newPos == '.' = Just $ grid // [(pos, '.'),(newPos, 'O')]
  | grid ! newPos == 'O' = (// [(pos, '.'),(newPos, 'O')]) <$> pushBox grid newPos dir
  | otherwise = Nothing
  where
    newPos = step dir pos

