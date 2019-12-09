module Computer where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Int as Int

-- day 7

type ComputerState = (Program, Inputs, Outputs, Pointer, RelativeBase)
type Program = Map.Map Int Int
type RelativeBase = Int
type Pointer = Int
type Inputs = [Int]
type Outputs = [Int]
type State = Map.Map Int ComputerState

stateful_process :: State -> Outputs
stateful_process state
  | pointer4 == (-1) =
    let (_, inputs, _, _, _) = state_lookup 0 state
    in inputs
  | otherwise =
    let newState0 = Map.map (\s -> process_until_halt s) state
        newState1 = fill_inputs newState0 4
    in stateful_process newState1
  where (_, _, outputs4, pointer4, rb4) = state_lookup 4 state

process_until_halt :: ComputerState -> ComputerState
process_until_halt (program, i, o, p, r)
  | opCode == 99 = (program, i, o, (-1), r)
  | (handle_op opCode (program, i, o, p, r)) == (program, i, o, p, r) =
      (program, i, o, p, r)
  | otherwise =
    let newState = handle_op opCode (program, i, o, p, r)
    in process_until_halt newState
  where opCode = program_lookup p program

fill_inputs :: State -> Int -> State
fill_inputs state (-1) = state
fill_inputs state n =
  let (p1, i1, outputs, po1, rb1) = state_lookup n state
      next = next_amp n
      oldComp = state_lookup next state
      (p, oldInputs, o, po, rb) = oldComp
      newComp = (p, outputs, o, po, rb)
      newState0 = Map.insert next newComp state
      newState1 = Map.insert n (p1, i1, [], po1, rb1) newState0
  in fill_inputs newState1 (n - 1)

next_amp :: Int -> Int
next_amp n
  | n == 4 = 0
  | otherwise = (n + 1)

maybe_jump :: Bool -> Int -> Int -> Int
maybe_jump True n val = val
maybe_jump False n val = n + 3

handle_op :: Int -> ComputerState -> ComputerState
handle_op opCode (program, inputs, outputs, pointer, relativeBase)
  | (op == 1) || (op == 2) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          mode3 = digit 3 opCode
          index1 = loc_and_rel_base mode1 (pointer + 1) program relativeBase
          val1 = program_lookup index1 program
          index2 = loc_and_rel_base mode2 (pointer + 2) program relativeBase
          val2 = program_lookup index2 program
          position = loc_and_rel_base mode3 (pointer + 3) program relativeBase
          value = do_calc val1 val2 op
          newProgram = Map.insert position value program
      in (newProgram, inputs, outputs, pointer + 4, relativeBase)
  | (op == 3) && (inputs == []) =
      (program, inputs, outputs, pointer, relativeBase)
  | op == 3 =
      let mode = digit 1 opCode
          position = loc_and_rel_base mode (pointer + 1) program relativeBase
          newProgram = Map.insert position (head inputs) program
      in (newProgram, tail inputs, outputs, pointer + 2, relativeBase)
  | op == 4 =
      let mode = digit 1 opCode
          position = loc_and_rel_base mode (pointer + 1) program relativeBase
          value = program_lookup position program
      in (program, inputs, outputs ++ [value], pointer + 2, relativeBase)
  | (op == 5) || (op == 6) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          location1 = loc_and_rel_base mode1 (pointer + 1) program relativeBase
          val1 = program_lookup location1 program
          location2 = loc_and_rel_base mode2 (pointer + 2) program relativeBase
          val2 = program_lookup location2 program
          should_jump = case op of
                          5 -> val1 /= 0
                          6 -> val1 == 0
          newPointer = maybe_jump should_jump pointer val2
      in (program, inputs, outputs, newPointer, relativeBase)
  | (op == 7) || (op == 8) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          mode3 = digit 3 opCode
          index1 = loc_and_rel_base mode1 (pointer + 1) program relativeBase
          val1 = program_lookup index1 program
          index2 = loc_and_rel_base mode2 (pointer + 2) program relativeBase
          val2 = program_lookup index2 program
          position = loc_and_rel_base mode3 (pointer + 3) program relativeBase
          value = get_value_op_7_8 op val1 val2
          newProgram = Map.insert position value program
      in (newProgram, inputs, outputs, pointer + 4, relativeBase)
  | (op == 9) =
      let mode = digit 1 opCode
          index = loc_and_rel_base mode (pointer + 1) program relativeBase
          val = program_lookup index program
      in (program, inputs, outputs, pointer + 2, relativeBase + val)
  | otherwise = (program, inputs, outputs, pointer, relativeBase)
  where op = opCode `rem` 10

get_value_op_7_8 :: Int -> Int -> Int -> Int
get_value_op_7_8 7 val1 val2
  | val1 < val2 = 1
  | otherwise = 0
get_value_op_7_8 8 val1 val2
  | val1 == val2 = 1
  | otherwise = 0

-- from right to left
digit :: Int -> Int -> Int
digit n number =
  (number `rem` (100 * (10 ^ n))) `div` (10 * (10 ^ n))

loc_and_rel_base :: Int -> Int -> Map.Map Int Int -> Int -> Int
loc_and_rel_base mode i map rb
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

-- part 1
process :: [Int] -> [Int] -> Outputs
process xs inputs =
  let zippedList = zip [0..] xs
      program = Map.fromList zippedList
      computerState = (program, inputs, [], 0, 0)
      (_, _, outputs, _, _) = process_until_halt computerState
  in outputs

-- do_process :: Map.Map Int Int ->
-- Int -> [Int] -> [Int] -> [Int]
-- do_process map n res inputs
--   | opCode == 99 = res
--   | otherwise =
--       let newN = next_pointer opCode n map
--           (newMap, newRes, newInputs) = handleOps opCode n map res inputs
--       in do_process newMap newN newRes newInputs
--   where opCode = map_lookup n map
