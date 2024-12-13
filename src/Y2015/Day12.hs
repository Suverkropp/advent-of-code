module Y2015.Day12 (day12) where

import AoC
import Data.Char (isDigit)
import Data.List.Extra (wordsBy)
import Text.Parsec (between, many, many1, parse, sepBy, (<|>))
import Text.Parsec.Char (char, noneOf, satisfy)
import Text.Parsec.String (Parser)
import Data.Tuple.Extra (second)

day12 :: AoC String
day12 =
  AoC
    { year = 2015,
      day = 12,
      handleInput = id,
      part1 = sum . map read . wordsBy (not . numChar),
      part2 = countNums . removeRed . parseElement
    }

numChar :: Char -> Bool
numChar b = isDigit b || b == '-'

data Element = List [Element] | Object [(String, Element)] | Number Int | StringEl String
  deriving (Eq)

parseElement :: String -> Element
parseElement s = case parse elementParser "" s of
  Left err -> error $ show err
  Right el -> el

elementParser :: Parser Element
elementParser = listParser <|> objectParser <|> numberParser <|> (StringEl <$> bareStringParser)

listParser :: Parser Element
listParser = List <$> between (char '[') (char ']') (elementParser `sepBy` char ',')

objectParser :: Parser Element
objectParser = Object <$> between (char '{') (char '}') (attributeParser `sepBy` char ',')

attributeParser :: Parser (String, Element)
attributeParser = do
  s <- bareStringParser
  char ':'
  el <- elementParser
  return (s, el)

numberParser :: Parser Element
numberParser = Number . read <$> many1 (satisfy numChar)

bareStringParser :: Parser String
bareStringParser = between (char '"') (char '"') (many $ noneOf ['"'])

removeRed :: Element -> Element
removeRed (List xs) = List $ map removeRed xs
removeRed (Object kvs) | any ((== StringEl "red") . snd) kvs = Object []
removeRed (Object kvs) = Object $ map (second removeRed) kvs
removeRed e = e

countNums :: Element -> Int
countNums (Number n) = n
countNums (List xs) = sum $ map countNums xs
countNums (Object kvs) = sum $ map (countNums . snd) kvs
countNums (StringEl _) = 0
