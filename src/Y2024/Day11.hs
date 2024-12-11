module Y2024.Day11 (day11, showBlinkTrie) where

import AoC
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Map.Lazy (Map, empty, insert, member, (!))
import Utils (applyNTimes)

day11 :: AoC [Int]
day11 =
  AoC
    { year = 2024,
      day = 11,
      handleInput = map read . words,
      part1 = length . applyNTimes 25 blink,
      part2 = sum . map (length . blinkStoneWithTrie 35)
    }

blink :: [Int] -> [Int]
blink = concatMap blinkStone

blinkStone :: Int -> [Int]
blinkStone 0 = [1]
blinkStone n | even $ length digits = map read [take half digits, drop half digits]
  where
    digits = show n
    half = length digits `div` 2
blinkStone n = [2024 * n]

type WithMap = State (Map (Int, Int) [Int])

fastBlinkN :: Int -> [Int] -> [Int]
fastBlinkN i ss = evalState (concat <$> mapM (fastBlinkStoneN i) ss) empty

fastBlinkStoneN :: Int -> Int -> WithMap [Int]
fastBlinkStoneN i s = do
  mem <- get
  if (i, s) `member` mem
    then
      return $ mem ! (i, s)
    else do
      -- blink once
      let blinked = blinkStone s
      -- compute the rest with fastBlinkStoneN
      res <- concat <$> mapM (fastBlinkStoneN (i - 1)) blinked
      -- put result in mem
      modify $ insert (i, s) res
      -- return result
      return res

data Trie a = Trie a (Trie a) (Trie a)

getFromTrie :: Int -> Trie a -> a
getFromTrie i = getFromTrie' (i + 1)
  where
    getFromTrie' 1 (Trie x _ _) = x
    getFromTrie' n (Trie _ l r)
      | even n = getFromTrie' (n `div` 2) l
      | otherwise = getFromTrie' ((n - 1) `div` 2) r

blinkTrie :: Trie [[Int]]
blinkTrie = blinkTrie' id
  where
    blinkTrie' f = Trie [blinkStoneWithTrie i (f 1 - 1) | i <- [0 .. 75]] (blinkTrie' (f . (* 2))) (blinkTrie' (f . (\s -> s * 2 + 1)))

blinkStoneWithTrie :: Int -> Int -> [Int]
blinkStoneWithTrie 0 s = [s]
blinkStoneWithTrie 1 s = blinkStone s
blinkStoneWithTrie i s = concatMap (\s' -> getFromTrie s' blinkTrie !! (i - 1)) $ blinkStone s
