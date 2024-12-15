module Y2024.Day15 (day15, pushBigBox) where

import AoC
import Data.Array (assocs, (!), (//))
import Data.Bifunctor (bimap, first, second)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Utils (Direction (..), Grid, Pos, isHorizontal, readGrid, step, toDirection)

day15 :: AoC (String, [Direction])
day15 =
  AoC
    { year = 2024,
      day = 15,
      handleInput = second (map toDirection) . splitOnNewline,
      part1 = sum . map getGPS . getBoxes 'O' . uncurry applyMoves . first readWarehouse,
      part2 = sum . map getGPS . getBoxes '[' . uncurry applyMoves . first (readWarehouse . doubleWidth)
    }

type Warehouse = (Grid Char, Pos)

readWarehouse :: String -> Warehouse
readWarehouse = getRobot . readGrid

splitOnNewline :: String -> (String, String)
splitOnNewline = bimap (init . unlines) concat . break null . lines

getRobot :: Grid Char -> (Grid Char, Pos)
getRobot grid = let pos = fst $ fromJust $ find ((== '@') . snd) $ assocs grid in (grid // [(pos, '.')], pos)

getGPS :: Pos -> Int
getGPS (x, y) = x + 100 * y

getBoxes :: Char -> Grid Char -> [Pos]
getBoxes c = map fst . filter ((== c) . snd) . assocs

applyMoves :: Warehouse -> [Direction] -> Grid Char
applyMoves w = fst . foldl applyMove w

applyMove :: Warehouse -> Direction -> Warehouse
applyMove (grid, pos) dir = case grid ! newPos of
  '.' -> (grid, newPos)
  'O' -> if isJust pushed then (fromJust pushed, newPos) else (grid, pos)
  '[' -> if isJust bigPushed then (fromJust bigPushed, newPos) else (grid, pos)
  ']' -> if isJust bigPushed then (fromJust bigPushed, newPos) else (grid, pos)
  '#' -> (grid, pos)
  _ -> undefined
  where
    newPos = step dir pos
    pushed = pushBox newPos dir grid
    bigPushed = pushBigBox newPos dir grid

pushBox :: Pos -> Direction -> Grid Char -> Maybe (Grid Char)
pushBox pos dir grid = case grid ! newPos of
  '.' -> Just $ grid // [(pos, '.'), (newPos, boxChar)]
  c | isBox c -> (// [(pos, '.'), (newPos, boxChar)]) <$> pushBox newPos dir grid
  '#' -> Nothing
  _ -> undefined
  where
    newPos = step dir pos
    isBox = (`elem` ['O', '[', ']'])
    boxChar = grid ! pos

pushBigBox :: Pos -> Direction -> Grid Char -> Maybe (Grid Char)
pushBigBox pos dir grid
  | isHorizontal dir = pushBox pos dir grid
  | otherwise = case (grid ! newLeftPos, grid ! newRightPos) of
      ('.', '.') -> Just $ moveBox grid
      ('.', '[') -> moveBox <$> pushBigBox newRightPos dir grid
      (']', '.') -> moveBox <$> pushBigBox newLeftPos dir grid
      (']', '[') -> moveBox <$> (pushBigBox newLeftPos dir grid >>= pushBigBox newRightPos dir)
      ('[', ']') -> moveBox <$> pushBigBox newLeftPos dir grid
      _ -> Nothing
  where
    (leftPos, rightPos) = if grid ! pos == '[' then (pos, step East pos) else (step West pos, pos)
    newLeftPos = step dir leftPos
    newRightPos = step dir rightPos
    moveBox = (// [(leftPos, '.'), (newLeftPos, '['), (rightPos, '.'), (newRightPos, ']')])

doubleWidth :: String -> String
doubleWidth = concatMap doubleChar
  where
    doubleChar '#' = "##"
    doubleChar '.' = ".."
    doubleChar 'O' = "[]"
    doubleChar '@' = "@."
    doubleChar '\n' = "\n"
    doubleChar _ = undefined
