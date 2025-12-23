module Y2024.Day21 (day21, complexity, finalMoves, codeToMoves, movesToMoves) where

import AoC
import Utils
import Control.Monad (zipWithM)

day21 :: AoC [String] Int Int
day21 =
  AoC
    { year = 2024,
      day = 21,
      handleInput = lines,
      part1 = sum . map complexity,
      part2 = undefined
    }

data Move = UpMove | DownMove | LeftMove | RightMove | A deriving (Eq)

instance Show Move where
  show UpMove = "^"
  show DownMove = "v"
  show LeftMove = "<"
  show RightMove = ">"
  show A = "A"

complexity :: String -> Int
complexity code = minimum (length <$> finalMoves code) * read (init code)

finalMoves :: String -> [[Move]]
finalMoves code = codeToMoves code >>= movesToMoves >>= movesToMoves

codeToMoves :: String -> [[Move]]
codeToMoves code = map concat $ zipWithM movesAndPress positions (tail positions)
  where
    positions = map codeToPos ('A' : code)

movesToMoves :: [Move] -> [[Move]]
movesToMoves moves = map concat $ zipWithM movesAndPress positions (tail positions)
  where
    positions = map moveToPos (A : moves)

movesAndPress :: Pos -> Pos -> [[Move]]
movesAndPress (x, y) (x', y') = if x == x' || y == y' then [xMoves ++ yMoves ++ [A]] else [xMoves ++ yMoves ++ [A], yMoves ++ xMoves ++ [A]]
  where
    xMoves = if x <= x' then replicate (x' - x) RightMove else replicate (x - x') LeftMove
    yMoves = if y <= y' then replicate (y' - y) DownMove else replicate (y - y') DownMove

codeToPos :: Char -> Pos
codeToPos '0' = (1, 3)
codeToPos '1' = (0, 2)
codeToPos '2' = (1, 2)
codeToPos '3' = (2, 2)
codeToPos '4' = (0, 1)
codeToPos '5' = (1, 1)
codeToPos '6' = (2, 1)
codeToPos '7' = (0, 0)
codeToPos '8' = (1, 0)
codeToPos '9' = (2, 0)
codeToPos 'A' = (2, 3)
codeToPos _ = error "Invalid code"

moveToPos :: Move -> Pos
moveToPos UpMove = (1, 0)
moveToPos DownMove = (1, 1)
moveToPos LeftMove = (0, 1)
moveToPos RightMove = (2, 1)
moveToPos A = (2, 0)
