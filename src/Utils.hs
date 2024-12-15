module Utils
  ( Grid,
    readGrid,
    Direction (..),
    toDirection,
    min3,
    step,
    Pos,
    applyNTimes,
    intParserPos,
    intParser,
    add,
    mul,
    posParser,
    showGrid,
    numChar,
    getNums,
  )
where

import Data.Array (Array, bounds, (!), listArray)
import Data.Char (isDigit)
import Text.Parsec (digit, many1, oneOf, option, spaces, string)
import Text.Parsec.String (Parser)
import Data.List.Extra (wordsBy)
import Data.List (transpose)

data Direction = North | East | South | West
  deriving (Eq, Ord)

type Pos = (Int, Int)

infixl 6 `add`

add :: Pos -> Pos -> Pos
add (x, y) (x', y') = (x + x', y + y')

infixl 7 `mul`

mul :: Int -> Pos -> Pos
mul i (x, y) = (i * x, i * y)

toDirection :: Char -> Direction
toDirection '^' = North
toDirection '>' = East
toDirection 'v' = South
toDirection '<' = West
toDirection _ = undefined

step :: Direction -> Pos -> Pos
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

type Grid a = Array Pos a

readGrid :: String -> Grid Char
readGrid s = listArray ((0,0),(mx-1,my-1)) $ concat . transpose $ lines s
  where
    my = length $ lines s
    mx = length s `div` my

showGrid :: Grid Char -> String
showGrid grid = foldr1 concatLines [[grid ! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    concatLines a b = a ++ "\n" ++ b
    ((minX, minY), (maxX, maxY)) = bounds grid

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = min (min a b) c

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ a = a
applyNTimes n f a = applyNTimes (n - 1) f (f a)

intParserPos :: Parser Int
intParserPos = read <$> many1 digit

intParser :: Parser Int
intParser = do
  spaces
  sign <- option ' ' (oneOf "-+")
  num <- many1 digit
  return $ read (sign : num)

numChar :: Char -> Bool
numChar b = isDigit b || b == '-' || b == '+'

getNums :: String -> [Int]
getNums = map read . wordsBy (not . numChar)

posParser :: Parser Pos
posParser = do
  x <- intParser
  _ <- string ","
  y <- intParser
  return (x, y)
