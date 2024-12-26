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
    isHorizontal,
    isVertical,
    findInGrid,
    turnRight,
    turnLeft,
    readPos,
    showBoolGrid,
    binarySearch,
    allSteps,
    mapToSnd,
  )
where

import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Char (isDigit)
import Data.List (find, transpose)
import Data.List.Extra (wordsBy)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both, second)
import Text.Parsec (digit, many1, oneOf, option, spaces, string)
import Text.Parsec.String (Parser)

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

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

step :: Direction -> Pos -> Pos
step North (x, y) = (x, y - 1)
step East (x, y) = (x + 1, y)
step South (x, y) = (x, y + 1)
step West (x, y) = (x - 1, y)

allSteps :: Pos -> [Pos]
allSteps (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isVertical :: Direction -> Bool
isVertical d = d == North || d == South

isHorizontal :: Direction -> Bool
isHorizontal d = d == East || d == West

type Grid a = Array Pos a

readGrid :: String -> Grid Char
readGrid s = listArray ((0, 0), (mx - 1, my - 1)) $ concat . transpose $ lines s
  where
    my = length $ lines s
    mx = length $ head $ lines s

showGrid :: Grid Char -> String
showGrid grid = foldr1 concatLines [[grid ! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    concatLines a b = a ++ "\n" ++ b
    ((minX, minY), (maxX, maxY)) = bounds grid

showBoolGrid :: Grid Bool -> String
showBoolGrid = showGrid . fmap (\x -> if x then '#' else '.')

findInGrid :: (Eq a) => a -> Grid a -> Pos
findInGrid x = fst . fromJust . find ((== x) . snd) . assocs

min3 :: (Ord a) => a -> a -> a -> a
min3 a b c = min a (min b c)

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

readPos :: String -> Pos
readPos = both read . second tail . span numChar

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch func minN maxN
  | maxN == minN + 1 = maxN
  | func middle = binarySearch func minN middle
  | otherwise = binarySearch func middle maxN
  where
    middle = (minN + maxN) `div` 2

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)
