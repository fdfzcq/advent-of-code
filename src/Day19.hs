module Day19 where

import Computer
import qualified Data.List as List
import Debug.Trace

max_x = 50
max_y = 50

part1 :: [Int] -> Int
part1 program =
  let cs = Computer.initial_computer_state program []
      res = process_coordinates_and_get_outputs (0, 0) cs 0
  in res

process_coordinates_and_get_outputs :: (Int, Int) -> Computer.ComputerState -> Int -> Int
process_coordinates_and_get_outputs (x, y) cs res
  | y > max_y = res
  | x > max_x = process_coordinates_and_get_outputs (0, y + 1) cs res
  | otherwise =
    let cs' = cs {inputs = [x, y]}
        cs'' = Computer.process_until_halt cs'
        res' = res + head (outputs cs'')
    in trace
       ("x: " ++ show x ++ " y: " ++ show y ++ " res: " ++ show res)
       (process_coordinates_and_get_outputs (x + 1, y) cs res')

part2 :: [Int] -> Int -> (Int, Int)
part2 program y =
  let cs = Computer.initial_computer_state program []
      x = find_x y
      (x', y') = find_closest_coordinate (x, y) cs
  in (x', y')

find_x :: Int -> Int
find_x y = (y `div` 3) * 2

find_closest_coordinate :: (Int, Int) -> Computer.ComputerState -> (Int, Int)
find_closest_coordinate (x, y) cs
  | a == 0 && b == 0 = find_closest_coordinate ((x + 1), (y + 1)) cs
  | a == 0 = find_closest_coordinate (x, (y + 1)) cs
  | b == 0 = find_closest_coordinate ((x + 1), y) cs
  | otherwise =
    (x, y)
  where ([a], [b]) =
          trace
          ( "x: " ++ show x ++ " y: " ++ show y )
          ( outputs (Computer.process_until_halt (cs {inputs = [x + 99, y]}))
          , outputs (Computer.process_until_halt (cs {inputs = [x, y + 99]})))
