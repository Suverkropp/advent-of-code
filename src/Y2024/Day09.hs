module Y2024.Day09 (day9) where

import AoC
import Data.Char (digitToInt)
import Data.List (mapAccumL)
import Data.Tuple.Extra (first, fst3)

day9 :: AoC [Int]
day9 =
  AoC
    { year = 2024,
      day = 9,
      handleInput = map digitToInt,
      part1 = sum . zipWith (*) [0 ..] . compressDiskByBlock . diskMapToBlocks,
      part2 = sum . map fileChecksum . compressByFiles
    }

diskMapToBlocks :: [Int] -> [Int]
diskMapToBlocks = concat . snd . mapAccumL func (True, 0)
  where
    func :: (Bool, Int) -> Int -> ((Bool, Int), [Int])
    func (True, i) n = ((False, i), replicate n i)
    func (False, i) n = ((True, i + 1), replicate n (-1))

compressDiskByBlock :: [Int] -> [Int]
compressDiskByBlock disk = go indexedDisk (reverse indexedDisk)
  where
    indexedDisk = zip disk [0 ..]
    go :: [(Int, Int)] -> [(Int, Int)] -> [Int]
    go ((c, i) : xs) ((d, j) : ys)
      | i >= j = [c | c /= -1 && i == j]
      | c /= -1 = c : go xs ((d, j) : ys)
      | d == -1 = go ((c, i) : xs) ys
      | otherwise = d : go xs ys
    go _ _ = error "list ran out"

-- file number, position, length
type File = (Int, Int, Int)

fileNums :: [Int]
fileNums = go 0
  where
    go i = i : -1 : go (i + 1)

addPositions :: [Int] -> [(Int, Int)]
addPositions = snd . mapAccumL (\pos len -> (pos + len, (pos, len))) 0

addPosAndFileNum :: [Int] -> [File]
addPosAndFileNum = zipWith (\f (p, l) -> (f, p, l)) fileNums . addPositions

compressByFiles :: [Int] -> [File]
compressByFiles diskMap = uncurry removeMoved $ moveFiles toMove completeMap
  where
    completeMap = addPosAndFileNum diskMap
    toMove = reverse $ filter ((/= -1) . fst3) completeMap

moveFiles :: [File] -> [File] -> ([File], [File])
moveFiles [] diskMap = ([], diskMap)
moveFiles (file : toMove) diskMap = case moveFile file diskMap of
  Nothing -> moveFiles toMove diskMap
  Just diskMap' -> first (file :) $ moveFiles toMove diskMap'

moveFile :: File -> [File] -> Maybe [File]
moveFile (_, _, _) [] = Nothing
moveFile (f, p, l) ((f', p', l') : files)
  | p' >= p = Nothing
  | f' == -1 && l' > l = Just $ (f, p', l) : (f', p' + l, l' - l) : files
  | f' == -1 && l' == l = Just $ (f, p', l) : files
  | otherwise = ((f', p', l') :) <$> moveFile (f, p, l) files

removeMoved :: [File] -> [File] -> [File]
removeMoved moved = filter (`notElem` moved)

fileChecksum :: File -> Int
fileChecksum (-1, _, _) = 0
fileChecksum (f, p, l) = f * sum [p .. p + l - 1]
