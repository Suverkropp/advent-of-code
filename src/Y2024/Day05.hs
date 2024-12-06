module Day05
  ( part1,
    part2,
    handleInput,
  )
where

import Control.Monad (foldM)
import Data.List.Extra (stripInfix, wordsBy)
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)
import Data.List (sortBy)

type Rule = (Int, Int)

type Update = [Int]

handleInput :: String -> ([Rule], [Update])
handleInput = decodeLines . lines

decodeLines :: [String] -> ([Rule], [Update])
decodeLines = go []
  where
    go :: [Rule] -> [String] -> ([Rule], [Update])
    go rs [] = (rs, [])
    go rs (s : ss) = case readRule s of
      Nothing -> (rs, decodeUpdates ss)
      Just r -> go (r : rs) ss

decodeUpdates :: [String] -> [Update]
decodeUpdates = map (map read . wordsBy (== ','))

readRule :: String -> Maybe Rule
readRule = fmap (both read) . stripInfix "|"

part1 :: ([Rule], [Update]) -> Int
part1 = uncurry part1'
  where
    part1' rs = sum . map getMiddle . filter (isOrdered rs)

isOrdered :: [Rule] -> Update -> Bool
isOrdered rs = isJust . foldM func []
  where
    func xs x = if x `elem` xs then Nothing else Just (getRules x rs ++ xs)
    getRules x = map fst . filter ((== x) . snd)

getMiddle :: Update -> Int
getMiddle u = u !! (length u `div` 2)

part2 :: ([Rule], [Update]) -> Int
part2 = uncurry part2'
  where
    part2' rs = sum . map (getMiddle . orderBy rs) . filter (not . isOrdered rs)

orderBy :: [Rule] -> Update -> Update
orderBy rs = sortBy compareRule
  where
    compareRule a b = if (a,b) `elem` rs then LT else GT





