module Y2024.Day05 (day5) where

import AoC
import Control.Monad (foldM)
import Data.List (sortBy)
import Data.List.Extra (stripInfix, wordsBy)
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)

type Rule = (Int, Int)

type Update = [Int]

day5 :: AoC ([Rule], [Update])
day5 =
  AoC
    { year = 2024,
      day = 5,
      handleInput = readRulesUpdates,
      part1 = uncurry addOrderedMiddles,
      part2 = uncurry addUnorderedMiddles
    }

readRulesUpdates :: String -> ([Rule], [Update])
readRulesUpdates = decodeLines . lines

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

addOrderedMiddles :: [Rule] -> [Update] -> Int
addOrderedMiddles rs = sum . map getMiddle . filter (isOrdered rs)

isOrdered :: [Rule] -> Update -> Bool
isOrdered rs = isJust . foldM func []
  where
    func xs x = if x `elem` xs then Nothing else Just (getRules x rs ++ xs)
    getRules x = map fst . filter ((== x) . snd)

getMiddle :: Update -> Int
getMiddle u = u !! (length u `div` 2)

addUnorderedMiddles :: [Rule] -> [Update] -> Int
addUnorderedMiddles rs = sum . map (getMiddle . orderBy rs) . filter (not . isOrdered rs)

orderBy :: [Rule] -> Update -> Update
orderBy rs = sortBy compareRule
  where
    compareRule a b = if (a, b) `elem` rs then LT else GT
