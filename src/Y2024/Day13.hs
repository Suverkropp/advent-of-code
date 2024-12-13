{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2024.Day13 (day13, Machine (..)) where

import AoC
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Text.Parsec (endOfLine, many, optional, parse, string)
import Text.Parsec.String (Parser)
import Utils (Pos, add, intParserPos, mul)

day13 :: AoC [Machine]
day13 =
  AoC
    { year = 2024,
      day = 13,
      handleInput = parseMachines,
      part1 = sum . mapMaybe getPrize,
      part2 = sum . mapMaybe (fastGetPrize . movePrize)
    }

data Machine = Machine
  { a :: Pos,
    b :: Pos,
    prize :: Pos
  }

machineParser :: Parser Machine
machineParser = do
  optional $ many endOfLine
  string "Button A: X+"
  aX <- intParserPos
  string ", Y+"
  aY <- intParserPos
  endOfLine
  string "Button B: X+"
  bX <- intParserPos
  string ", Y+"
  bY <- intParserPos
  endOfLine
  string "Prize: X="
  prizeX <- intParserPos
  string ", Y="
  prizeY <- intParserPos
  return $ Machine {a = (aX, aY), b = (bX, bY), prize = (prizeX, prizeY)}

parseMachines :: String -> [Machine]
parseMachines s = case parse (many machineParser) "" s of
  Left err -> error $ show err
  Right ms -> ms

getOptions :: Machine -> [[(Int, Pos)]]
getOptions m = map (takeWhile smaller) $ takeWhile (smaller . head) options
  where
    options = [[(ai * 3 + bi, ai `mul` a m `add` bi `mul` b m) | bi <- [0 ..]] | ai <- [0 ..]]
    smaller (_, (x, y)) = let (mx, my) = prize m in x <= mx && y <= my

getPrize :: Machine -> Maybe Int
getPrize m = maybeMinimum $ map fst $ mapMaybe (find isPrize) $ getOptions m
  where
    isPrize :: (Int, Pos) -> Bool
    isPrize (_, p) = p == prize m

maybeMinimum :: [Int] -> Maybe Int
maybeMinimum [] = Nothing
maybeMinimum [x] = Just x
maybeMinimum (x : y : rest) = maybeMinimum (min x y : rest)

movePrize :: Machine -> Machine
movePrize m = Machine {a = a m, b = b m, prize = prize m `add` (10000000000000, 10000000000000)}

-- using change of basis in linear algebra
fastGetPrize :: Machine -> Maybe Int
fastGetPrize m =
  if aNum `mod` det == 0 && bNum `mod` det == 0
    then Just (3 * aNum `div` det + bNum `div` det)
    else Nothing
  where
    (xa, ya) = a m
    (xb, yb) = b m
    (x, y) = prize m
    det = xa * yb - xb * ya
    aNum = x * yb - y * xb
    bNum = y * xa - x * ya

isDifficult :: Machine -> Bool
isDifficult m = xa % ya == xb % yb
  where
    (xa, ya) = a m
    (xb, yb) = b m
