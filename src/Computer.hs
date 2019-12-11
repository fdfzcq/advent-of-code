module Computer where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Int as Int

-- day 7

data ComputerState = ComputerState
  { program :: Program
  , inputs :: Inputs
  , outputs :: Outputs
  , pointer :: Pointer
  , relativeBase :: RelativeBase
  } deriving Show

type Program = Map.Map Int Int
type RelativeBase = Int
type Pointer = Int
type Inputs = [Int]
type Outputs = [Int]

type State = Map.Map Int ComputerState

init_index = 0
last_index = 4

-- used in Day7 --

stateful_process :: State -> Outputs
stateful_process state
  | pointer cs4 == (-1) =
    let cs = state_lookup init_index state
    in inputs cs
  | otherwise =
    let newState0 = Map.map (\s -> process_until_halt s) state
        newState1 = fill_inputs newState0 last_index
    in stateful_process newState1
  where cs4 = state_lookup last_index state

fill_inputs :: State -> Int -> State
fill_inputs state (-1) = state
fill_inputs state n =
  let cs1 = state_lookup n state
      next = next_amp n
      oldComp = state_lookup next state
      oldCs = oldComp
      newComp = oldComp {inputs = (outputs cs1)}
      newState0 = Map.insert next newComp state
      newState1 = Map.insert n (cs1 {outputs = []}) newState0
  in fill_inputs newState1 (n - 1)

-------------------

process_until_halt :: ComputerState -> ComputerState
process_until_halt cs
  | (program cs == program newCs) && (pointer cs == pointer newCs) = cs
  | (pointer cs) == (-1) = cs
  | otherwise = process_until_halt newCs
  where opCode = program_lookup (pointer cs) (program cs)
        newCs = handle_op opCode cs

handle_op :: Int -> ComputerState -> ComputerState
handle_op opCode cs
  | opCode == 99 =
      cs {pointer = (-1)}
  | (op == 1) || (op == 2) =
      let val1 = read_value opCode cs 1
          val2 = read_value opCode cs 2
          value = do_calc val1 val2 op
          newProgram = write_value value opCode cs 3
      in cs { pointer = (pointer cs) + 4
            , program = newProgram }
  | (op == 3) && ((inputs cs) == []) =
      cs
  | op == 3 =
      let input = head (inputs cs)
          newProgram = write_value input opCode cs 1
      in cs { program = newProgram
            , inputs = tail (inputs cs)
            , pointer = (pointer cs) + 2 }
  | op == 4 =
      let value = read_value opCode cs 1
      in cs { outputs = (outputs cs) ++ [value]
            , pointer = (pointer cs) + 2 }
  | (op == 5) || (op == 6) =
      let val1 = read_value opCode cs 1
          val2 = read_value opCode cs 2
          should_jump = case op of
                          5 -> val1 /= 0
                          6 -> val1 == 0
          newPointer = maybe_jump should_jump (pointer cs) val2
      in cs { pointer = newPointer }
  | (op == 7) || (op == 8) =
      let val1 = read_value opCode cs 1
          val2 = read_value opCode cs 2
          value = binary_op op val1 val2
          newProgram = write_value value opCode cs 3
      in cs { program = newProgram
            , pointer = (pointer cs) + 4 }
  | (op == 9) =
      let val = read_value opCode cs 1
      in cs { pointer = (pointer cs) + 2
            , relativeBase = (relativeBase cs) + val }
  | otherwise = cs
  where op = opCode `rem` 10

binary_op :: Int -> Int -> Int -> Int
binary_op 7 val1 val2
  | val1 < val2 = 1
  | otherwise = 0
binary_op 8 val1 val2
  | val1 == val2 = 1
  | otherwise = 0

-- from right to left
digit :: Int -> Int -> Int
digit n number =
  (number `rem` (100 * (10 ^ n))) `div` (10 * (10 ^ n))

locate :: Int -> Int -> Map.Map Int Int -> Int -> Int
locate mode i map rb
  | mode == 0 = program_lookup i map
  | mode == 1 = i
  | mode == 2 = rb + (program_lookup i map)

do_calc :: Int -> Int -> Int -> Int
do_calc a1 a2 t
  | t == 1 = a1 + a2
  | t == 2 = a1 * a2

state_lookup :: Int -> Map.Map Int ComputerState -> ComputerState
state_lookup n map =
  let (Just cs) = Map.lookup n map
  in cs

program_lookup :: Int -> Map.Map Int Int -> Int
program_lookup n map  =
  Maybe.fromMaybe 0 (Map.lookup n map)

next_amp :: Int -> Int
next_amp n
  | n == 4 = 0
  | otherwise = (n + 1)

maybe_jump :: Bool -> Int -> Int -> Int
maybe_jump True n val = val
maybe_jump False n val = n + 3

read_value :: Int -> ComputerState -> Int -> Int
read_value opCode cs i =
  let mode = digit i opCode
      point = pointer cs
      index = locate mode (point + i) (program cs) (relativeBase cs)
  in program_lookup index (program cs)

write_value :: Int -> Int -> ComputerState -> Int -> Program
write_value value opCode cs i =
  let mode = digit i opCode
      point = pointer cs
      index = locate mode (point + i) (program cs) (relativeBase cs)
  in Map.insert index value (program cs)

initial_computer_state :: [Int] -> [Int] -> ComputerState
initial_computer_state xs inputs =
  let zippedList = zip [0..] xs
      prog = Map.fromList zippedList
  in ComputerState { program = prog
                   , inputs = inputs
                   , outputs = []
                   , pointer = 0
                   , relativeBase = 0 }

-- part 1
process :: [Int] -> [Int] -> Outputs
process xs inputs =
  let computerState = initial_computer_state xs inputs
      newCs = process_until_halt computerState
  in outputs newCs

-- do_process :: Map.Map Int Int ->
-- Int -> [Int] -> [Int] -> [Int]
-- do_process map n res inputs
--   | opCode == 99 = res
--   | otherwise =
--       let newN = next_pointer opCode n map
--           (newMap, newRes, newInputs) = handleOps opCode n map res inputs
--       in do_process newMap newN newRes newInputs
--   where opCode = map_lookup n map
