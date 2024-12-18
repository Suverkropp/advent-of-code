{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2015.Day19 (day19) where

import AoC
import Data.Bifunctor (bimap)
import Data.List.Extra (splitOn, nubOrd)
import Debug.Trace

day19 :: AoC ([Rule], Molecule) Int Int
day19 =
  AoC
    { year = 2015,
      day = 19,
      handleInput = bimap (map readRule) (!! 1). break null . lines,
      part1 = length . uncurry applyRules,
      part2 = uncurry search
    }

type Rule = (String, String)
type Molecule = String

readRule :: String -> Rule
readRule s = let [a,b] = splitOn " => " s in (a,b)

applyRules :: [Rule] -> Molecule -> [Molecule]
applyRules rules molecule = nubOrd $ concatMap (applyRule molecule) rules

applyRule :: Molecule -> Rule -> [Molecule]
applyRule molecule (a,b) = snd . foldl addPart (head parts, []) $ tail parts
  where
    parts = splitOn a molecule
    addPart :: (Molecule, [Molecule]) -> String -> (Molecule, [Molecule])
    addPart (withoutB, withBs) part = (withoutB ++ a ++ part, (withoutB ++ b ++ part) : map (++ a ++ part) withBs)

search :: [Rule] -> Molecule -> Int
search rules target = go 0 ["e"]
  where
    go :: Int -> [Molecule] -> Int
    go steps molecules
      | target `elem` molecules = steps
      | otherwise = go (traceShowId $ steps + 1) $ nubOrd $ concatMap (applyRules rules) molecules
