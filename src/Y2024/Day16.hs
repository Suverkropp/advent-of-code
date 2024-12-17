module Y2024.Day16 (day16) where

import AoC
import Data.Array ((!))
import Data.Maybe (catMaybes, fromJust)
import Data.PSQueue (Binding ((:->)), PSQ, minView, singleton, insertWith)
import Data.Tuple.Extra ((&&&))
import Utils (Direction (..), Grid, Pos, findInGrid, readGrid, step, turnLeft, turnRight)

day16 :: AoC (Maze, Pos)
day16 =
  AoC
    { year = 2024,
      day = 16,
      handleInput = ((getWalls &&& findInGrid 'E') &&& findInGrid 'S') . readGrid,
      part1 = \(maze, pos) -> searchCheapestPath maze $ initialPSQ pos,
      part2 = undefined
    }

-- walls and end
type Maze = (Grid Bool, Pos)

type State = Binding (Pos, Direction) Int

type Priority = PSQ (Pos, Direction) Int

getWalls :: Grid Char -> Grid Bool
getWalls = fmap (== '#')

initialPSQ :: Pos -> Priority
initialPSQ p = singleton (p, East) 0

searchCheapestPath :: Maze -> Priority -> Int
searchCheapestPath maze prio = case allSteps maze state of
  Left cost -> cost
  Right states -> searchCheapestPath maze $ addAll states rest
  where
    (state, rest) = fromJust $ minView prio

addAll :: [State] -> Priority -> Priority
addAll states psq = foldr insertBinding psq states
  where
    insertBinding (a :-> b) = insertWith min a b

allSteps :: Maze -> State -> Either Int [State]
allSteps maze state@((pos, dir) :-> cost)
  | pos == snd maze = Left cost
  | otherwise =
      Right $
        catMaybes
          [ forwardStep maze state,
            forwardStep maze ((pos, turnRight dir) :-> cost + 1000),
            forwardStep maze ((pos, turnLeft dir) :-> cost + 1000)
          ]

forwardStep :: Maze -> State -> Maybe State
forwardStep maze ((pos, dir) :-> cost)
  | fst maze ! newPos = Nothing
  | otherwise = Just ((newPos, dir) :-> cost + 1)
  where
    newPos = step dir pos
