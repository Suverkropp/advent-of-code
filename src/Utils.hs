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
    mul
  )
where

import Data.Array (Array, array)
import Text.Parsec (digit, many1, oneOf, option, spaces)
import Text.Parsec.String (Parser)

data Direction = North | East | South | West
  deriving (Eq, Ord)

type Pos = (Int, Int)

infixl 6 `add`
add :: Pos -> Pos -> Pos
add (x, y) (x', y') = (x + x', y + y')

infixl 7 `mul`
mul :: Int -> Pos -> Pos
mul i (x,y) = (i*x,i*y)

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
readGrid s = array ((0, 0), (mx - 1, my - 1)) [((x, y), c) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l]
  where
    my = length $ lines s
    mx = length s `div` my

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
