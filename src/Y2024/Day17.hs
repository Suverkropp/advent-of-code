{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Y2024.Day17 (day17, runProgram) where

import AoC
import Control.Monad (foldM)
import Control.Monad.Trans.State (StateT (StateT, runStateT), gets, modify)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Bits (xor)
import Data.List.Extra ((!?))
import Utils (getNums)
import Debug.Trace

day17 :: AoC ProgramState
day17 =
  AoC
    { year = 2024,
      day = 17,
      handleInput = readInput,
      part1 = sum . runProgram,
      part2 = head . fastFindQuine . instructions
    }

data ProgramState = ProgramState
  { registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    instructions :: [Int],
    programCounter :: Int
  }

type Interpreter a = StateT ProgramState (Writer [Int]) a

readInput :: String -> ProgramState
readInput s = ProgramState {registerA = a, registerB = b, registerC = c, instructions = instrs, programCounter = 0}
  where
    a : b : c : instrs = getNums s

runProgram :: ProgramState -> [Int]
runProgram = execWriter . runStateT interpreter

interpreter :: Interpreter ()
interpreter = do
  maybeInstr <- getInstruction
  maybeArg <- getInstruction
  case (maybeInstr, maybeArg) of
    (Nothing, Nothing) -> return ()
    (Just instr, Just arg) -> runInstruction instr arg >> interpreter
    _ -> error "unexpected end of instructions"

getInstruction :: Interpreter (Maybe Int)
getInstruction = do
  instrs <- gets instructions
  pc <- gets programCounter
  modify $ \ps -> ps {programCounter = pc + 1}
  return $ instrs !? pc

runInstruction :: Int -> Int -> Interpreter ()
runInstruction 0 arg = do
  numenator <- gets registerA
  denominator <- (2 ^) <$> getCombo arg
  setRegisterA $ numenator `div` denominator
runInstruction 1 arg = do
  b <- gets registerB
  setRegisterB $ b `xor` arg
runInstruction 2 arg = do
  x <- getCombo arg
  setRegisterB $ x `mod` 8
runInstruction 3 arg = do
  a <- gets registerA
  if a == 0
    then return ()
    else modify $ \ps -> ps {programCounter = arg}
runInstruction 4 _ = do
  b <- gets registerB
  c <- gets registerC
  setRegisterB $ b `xor` c
runInstruction 5 arg = do
  val <- getCombo arg
  output $ val `mod` 8
runInstruction 6 arg = do
  numenator <- gets registerA
  denominator <- (2 ^) <$> getCombo arg
  setRegisterB $ numenator `div` denominator
runInstruction 7 arg = do
  numenator <- gets registerA
  denominator <- (2 ^) <$> getCombo arg
  setRegisterC $ numenator `div` denominator
runInstruction opcode _ = error $ "opcode " ++ show opcode ++ " unrecognised"

output :: Int -> Interpreter ()
output x = StateT $ \ps -> tell [x] >> return ((), ps)

getCombo :: Int -> Interpreter Int
getCombo x | 0 <= x && x <= 3 = return x
getCombo 4 = gets registerA
getCombo 5 = gets registerB
getCombo 6 = gets registerC
getCombo 7 = error "reserved combo operand: not allowed"
getCombo x = error $ "number " ++ show x ++ " not between 0 and 7"

setRegisterA :: Int -> Interpreter ()
setRegisterA x = modify $ \ps -> ps {registerA = x}

setRegisterB :: Int -> Interpreter ()
setRegisterB x = modify $ \ps -> ps {registerB = x}

setRegisterC :: Int -> Interpreter ()
setRegisterC x = modify $ \ps -> ps {registerC = x}

-- this is way too slow, since the anwser is > 8^15 = 2^45
searchQuine :: ProgramState -> Int
searchQuine ps = head [i | i <- [0 ..], checkQuine (ps {registerA = i})]

checkQuine :: ProgramState -> Bool
checkQuine ps = instructions ps == runProgram ps

runOneLoop :: Int -> Int
runOneLoop a =
  let b = (a `mod` 8) `xor` 1
      c = (a `div` (2 ^ b)) `mod` 8
   in 4 `xor` b `xor` c

fastQuineLoop :: Int -> Int -> [Int]
fastQuineLoop i n = filter (\x -> runOneLoop x == n) [i * 8 .. i * 8 + 7]

fastFindQuine :: [Int] -> [Int]
fastFindQuine = foldM fastQuineLoop 0 . reverse
