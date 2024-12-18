{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2015.Day06 (day6) where

import AoC
import Data.Array (accum, elems, listArray, (//))
import Data.Ix (range)
import Text.Parsec (Parsec (), char, digit, many1, parse, space, string, (<|>), string')
import Utils (Grid, Pos)

day6 :: AoC [Instruction] Int Int
day6 =
  AoC
    { year = 2015,
      day = 6,
      handleInput = map readInstruction . lines,
      part1 = length . filter id . elems . followInstructions,
      part2 = sum . elems . followNewInstructions
    }

data Instruction = Toggle Pos Pos | TurnOn Pos Pos | TurnOff Pos Pos

instructionParser :: Parsec String () Instruction
instructionParser = parseToggle <|> parseTurnOn <|> parseTurnOff
  where
    parseToggle = parseOneInstruction "toggle" Toggle
    parseTurnOn = parseOneInstruction "turn on" TurnOn
    parseTurnOff = parseOneInstruction "turn off" TurnOff

parseOneInstruction :: String -> (Pos -> Pos -> Instruction) -> Parsec String () Instruction
parseOneInstruction s f = do
  string' s
  space
  p1 <- parsePos
  string " through "
  p2 <- parsePos
  return $ f p1 p2

parsePos :: Parsec String () Pos
parsePos = do
  p1 <- read <$> many1 digit
  char ','
  p2 <- read <$> many1 digit
  return (p1, p2)

readInstruction :: String -> Instruction
readInstruction s = case parse instructionParser "" s of
  Left err -> error $ show err
  Right i -> i

followInstructions :: [Instruction] -> Grid Bool
followInstructions = foldl applyInstruction (emptyGrid False)

emptyGrid :: a -> Grid a
emptyGrid x = listArray ((0, 0), (999, 999)) (repeat x)

applyInstruction :: Grid Bool -> Instruction -> Grid Bool
applyInstruction g (TurnOn p1 p2) = g // [(i, True) | i <- range (p1, p2)]
applyInstruction g (TurnOff p1 p2) = g // [(i, False) | i <- range (p1, p2)]
applyInstruction g (Toggle p1 p2) = accum (\b _ -> not b) g [(i, ()) | i <- range (p1, p2)]

followNewInstructions :: [Instruction] -> Grid Int
followNewInstructions = foldl applyNewInstruction (emptyGrid 0)

applyNewInstruction :: Grid Int -> Instruction -> Grid Int
applyNewInstruction g (TurnOn p1 p2) = accum (\b _ -> b + 1) g [(i, ()) | i <- range (p1, p2)]
applyNewInstruction g (TurnOff p1 p2) = accum (\b _ -> (b - 1) `max` 0) g [(i, ()) | i <- range (p1, p2)]
applyNewInstruction g (Toggle p1 p2) = accum (\b _ -> b + 2) g [(i, ()) | i <- range (p1, p2)]
