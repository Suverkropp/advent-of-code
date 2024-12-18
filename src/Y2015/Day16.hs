module Y2015.Day16 (day16) where

import AoC
import Text.Parsec (lower, many, optional, parse, string)
import Text.Parsec.String (Parser)
import Utils (intParser)

day16 :: AoC [Sue] Int Int
day16 =
  AoC
    { year = 2015,
      day = 16,
      handleInput = map parseSue . lines,
      part1 = fst . head . filter matchesGift,
      part2 = fst . head . filter realMatchesGift
    }

type Sue = (Int, [(String, Int)])

parseSue :: String -> Sue
parseSue s = case parse sueParser "" s of
  Left err -> error $ show err
  Right sue -> sue

sueParser :: Parser Sue
sueParser = do
  _ <- string "Sue "
  i <- intParser
  _ <- string ": "
  attrs <- many attributeParser
  return (i, attrs)

attributeParser :: Parser (String, Int)
attributeParser = do
  optional $ string ", "
  s <- many lower
  _ <- string ": "
  i <- intParser
  return (s, i)

matchesGift :: Sue -> Bool
matchesGift = all (`elem` dataFromGift) . snd

dataFromGift :: [(String, Int)]
dataFromGift =
  [ ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)
  ]

realMatchesGift :: Sue -> Bool
realMatchesGift (_, attrs) = all match attrs
  where
    match ("cats", i) = i > 7
    match ("trees", i) = i > 3
    match ("pomeranians", i) = i < 3
    match ("goldfish", i) = i < 5
    match other = other `elem` dataFromGift
