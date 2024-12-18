{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2015.Day15 (day15) where

import AoC
import Utils (getNums)

day15 :: AoC [Ingredient] Int Int
day15 =
  AoC
    { year = 2015,
      day = 15,
      handleInput = map (toIngredient . getNums) . lines,
      part1 = maximum . map score . generateCookies,
      part2 = maximum . map score . filter has500Calories . generateCookies
    }

data Ingredient = Ingredient
  { capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

type Cookie = [(Int, Ingredient)]

toIngredient :: [Int] -> Ingredient
toIngredient [c, d, f, t, cal] = Ingredient c d f t cal

generateCookies :: [Ingredient] -> [Cookie]
generateCookies ings = map (`zip` ings) $ genCombinations 100 []
  where
    genCombinations :: Int -> [Int] -> [[Int]]
    genCombinations n xs
      | length xs == length ings - 1 = [n : xs]
      | otherwise = concatMap (\i -> genCombinations (n - i) (i : xs)) [0 .. n]

score :: Cookie -> Int
score c = product . map (max 0 . totalProp c) $ [capacity, durability, flavor, texture]

has500Calories :: Cookie -> Bool
has500Calories c = totalProp c calories == 500

totalProp :: Cookie -> (Ingredient -> Int) -> Int
totalProp c prop = sum $ map (\(n, ing) -> n * prop ing) c
