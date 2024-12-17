module Y2024.Day16 (day16) where

import AoC
import Data.Array ((!))
import Data.Containers.ListUtils (nubOrd)
import Data.Either (lefts)
import Data.Maybe (catMaybes, fromJust)
import Data.PSQueue (Binding ((:->)), PSQ, atMost, insertWith, minView, singleton)
import Data.Tuple.Extra ((&&&))
import Utils (Direction (..), Grid, Pos, findInGrid, readGrid, step, turnLeft, turnRight)

day16 :: AoC (Maze, Pos)
day16 =
  AoC
    { year = 2024,
      day = 16,
      handleInput = ((getWalls &&& findInGrid 'E') &&& findInGrid 'S') . readGrid,
      part1 = fst . uncurry searchCheapestPaths,
      part2 = length . nubOrd . concat . snd . uncurry searchCheapestPaths
    }

-- walls and end
type Maze = (Grid Bool, Pos)

type PosDir = (Pos, Direction)

type Cost = Int

type Path = [Pos]

type State = Binding PosDir (Cost, [Path])

type Priority = PSQ PosDir (Cost, [Path])

getWalls :: Grid Char -> Grid Bool
getWalls = fmap (== '#')

initialPSQ :: Pos -> Priority
initialPSQ p = singleton (p, East) (0, [[p]])

searchCheapestPaths :: Maze -> Pos -> (Cost, [Path])
searchCheapestPaths maze pos = search $ initialPSQ pos
  where
    search :: Priority -> (Cost, [Path])
    search prio = case allSteps maze state of
      Left (cost, paths) -> (cost, paths ++ findRestOfPaths maze cost prio)
      Right states -> search $ addAll states rest
      where
        (state, rest) = fromJust $ minView prio

findRestOfPaths :: Maze -> Cost -> Priority -> [Path]
findRestOfPaths maze cost = concatMap snd . lefts . map (allSteps maze) . atMost (cost + 1, [])

addAll :: [State] -> Priority -> Priority
addAll states psq = foldr insertBinding psq states
  where
    insertBinding (a :-> b) = insertWith combineLowest a b
    combineLowest (c1, ps1) (c2, ps2) = case compare c1 c2 of
      LT -> (c1, ps1)
      GT -> (c2, ps2)
      EQ -> (c1, ps1 ++ ps2)

allSteps :: Maze -> State -> Either (Cost, [Path]) [State]
allSteps maze state@((pos, dir) :-> (cost, paths))
  | pos == snd maze = Left (cost, paths)
  | otherwise =
      Right $
        catMaybes
          [ forwardStep maze state,
            forwardStep maze ((pos, turnRight dir) :-> (cost + 1000, paths)),
            forwardStep maze ((pos, turnLeft dir) :-> (cost + 1000, paths))
          ]

forwardStep :: Maze -> State -> Maybe State
forwardStep maze ((pos, dir) :-> (cost, paths))
  | fst maze ! newPos = Nothing
  | otherwise = Just ((newPos, dir) :-> (cost + 1, map (newPos :) paths))
  where
    newPos = step dir pos
