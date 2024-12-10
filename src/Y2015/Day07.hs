{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2015.Day07 (day7) where

import AoC
import Data.Bits (Bits (..), complement)
import Text.Parsec (digit, lower, many1, parse, space, string, try, (<|>))
import Text.Parsec.String (Parser)
import Data.Map.Lazy (Map, (!), fromList)

day7 :: AoC [Instruction]
day7 =
  AoC
    { year = 2015,
      day = 7,
      handleInput = map parseInstruction . lines,
      part1 = evaluate "a",
      part2 = evaluate "a" . replaceB
    }

data Instruction = Instruction Expression String

data Expression = Not Wire | And Wire Wire | Or Wire Wire | Lshift Wire Wire | Rshift Wire Wire | Simple Wire

data Wire = Static Int | Var String
  deriving (Eq)

parseInstruction :: String -> Instruction
parseInstruction s = case parse instructionParser "" s of
  Right instr -> instr
  Left err -> error $ show err

instructionParser :: Parser Instruction
instructionParser = do
  expr <- notParser <|> try binOrSimpleExprParser <|> (Simple <$> wireParser)
  space
  string "->"
  space
  Instruction expr <$> many1 lower

wireParser :: Parser Wire
wireParser =
  (Var <$> many1 lower)
    <|> (Static . read <$> many1 digit)

binOrSimpleExprParser :: Parser Expression
binOrSimpleExprParser = do
  w <- wireParser
  binOpParser w <|> return (Simple w)

binOpParser :: Wire -> Parser Expression
binOpParser w = do
  space
  binOp <-
    (string "AND" >> return And)
      <|> (string "OR" >> return Or)
      <|> (string "LSHIFT" >> return Lshift)
      <|> (string "RSHIFT" >> return Rshift)
  space
  w2 <- wireParser
  return $ binOp w w2

notParser :: Parser Expression
notParser = do
  string "NOT"
  space
  Not <$> wireParser

evaluate :: String -> [Instruction] -> Int
evaluate s instrs = varMap ! s
  where
    varMap :: Map String Int
    varMap = fromList [(v, evaluateExpr expr) | (Instruction expr v) <- instrs]

    evaluateExpr :: Expression -> Int
    evaluateExpr (Not w) = complement $ evaluateWire w
    evaluateExpr (And w1 w2) = evaluateWire w1 .&. evaluateWire w2
    evaluateExpr (Or w1 w2) = evaluateWire w1 .|. evaluateWire w2
    evaluateExpr (Lshift w1 w2) = evaluateWire w1 `shiftL` evaluateWire w2
    evaluateExpr (Rshift w1 w2) = evaluateWire w1 `shiftR` evaluateWire w2
    evaluateExpr (Simple w) = evaluateWire w

    evaluateWire :: Wire -> Int
    evaluateWire (Var v) = varMap ! v
    evaluateWire (Static x) = x

replaceB :: [Instruction] -> [Instruction]
replaceB = map replaceInstr
  where
   replaceInstr (Instruction _ "b") = Instruction (Simple (Static 16076)) "b"
   replaceInstr i = i
