module Day7 where

import qualified Data.List as List
import qualified Data.Map as Map
import Computer hiding (do_process, Program, Outputs, Inputs)

type Program = [Int]
type Phases = [Int]
type Outputs = [Int]
type Inputs = [Int]

initialInput = [0]

process :: Program -> Int
process program =
  let permutations = List.permutations [5..9]
  in maximum (do_process permutations program [])

do_process :: [Phases] -> Program -> Outputs -> [Int]
do_process [] program res = res
do_process (x:xs) program res =
  let initState = to_initial_state x program
      output = Computer.stateful_process initState
  in do_process xs program (head output:res)

to_initial_state :: Phases -> Program -> Map.Map Int Computer.ComputerState
to_initial_state phases program =
  let programMap = Map.fromList (zip [0..] program)
      stateList = List.map (\x -> init_state x programMap phases) [0..4]
  in Map.fromList stateList

init_state :: Int -> Map.Map Int Int -> [Int] -> (Int, ComputerState)
init_state 0 program phases =
  (0, (program, [phases!!0] ++ initialInput, [], 0, 0))
init_state n program phases =
  (n, (program, [phases!!n], [], 0, 0))

-- for part 1

run_program :: Phases -> Program -> Inputs -> Outputs
run_program phases program [] = []
run_program [] program inputs = inputs
run_program (x:xs) program inputs =
  let output = []
        -- Day5.process program ([x] ++ inputs)
  in run_program xs program output
