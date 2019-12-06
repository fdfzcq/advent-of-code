module Day5 where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

inputVal = 5

-- part 1
process :: [Int] -> [Int]
process xs =
  let zippedList = zip [0..] xs
      map = Map.fromList zippedList
  in do_process map 0 []

do_process :: Map.Map Int Int -> Int -> [Int] -> [Int]
do_process map n res
  | opCode == 99 = res
  | otherwise =
      let newN = next_pointer opCode n map
          (newMap, newRes) = handleOps opCode n map res
      in do_process newMap newN newRes
  where opCode = map_lookup n map

next_pointer :: Int -> Int -> Map.Map Int Int -> Int
next_pointer opCode n map
  | (op == 1) || (op == 2) = n + 4
  | (op == 3) || (op == 4) = n + 2
  | (op == 5) || (op == 6) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          location1 = location mode1 (n + 1) map
          val1 = map_lookup location1 map
          location2 = location mode2 (n + 2) map
          val2 = map_lookup location2 map
          should_jump = case op of
                          5 -> val1 /= 0
                          6 -> val1 == 0
      in maybe_jump should_jump n val2
  | (op == 7) || (op == 8) = n + 4
  where op = opCode `rem` 10

maybe_jump :: Bool -> Int -> Int -> Int
maybe_jump True n val = val
maybe_jump False n val = n + 3

handleOps :: Int -> Int -> Map.Map Int Int -> [Int] -> (Map.Map Int Int, [Int])
handleOps opCode n map res
  | (op == 1) || (op == 2) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          mode3 = digit 3 opCode
          index1 = location mode1 (n + 1) map
          val1 = map_lookup index1 map
          index2 = location mode2 (n + 2) map
          val2 = map_lookup index2 map
          position = location mode3 (n + 3) map
          value = do_calc val1 val2 op
          newMap = Map.insert position value map
      in (newMap, res)
  | op == 3 =
      let position = map_lookup (n + 1) map
          newMap = Map.insert position inputVal map
      in (newMap, res)
  | op == 4 =
      let mode = digit 1 opCode
          position = location mode (n + 1) map
          value = map_lookup position map
      in (map, (value:res))
  | (op == 7) || (op == 8) =
      let mode1 = digit 1 opCode
          mode2 = digit 2 opCode
          mode3 = digit 3 opCode
          index1 = location mode1 (n + 1) map
          val1 = map_lookup index1 map
          index2 = location mode2 (n + 2) map
          val2 = map_lookup index2 map
          position = location mode3 (n + 3) map
          value = get_value_op_7_8 op val1 val2
          newMap = Map.insert position value map
      in (newMap, res)
  | otherwise = (map, res)
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

location :: Int -> Int -> Map.Map Int Int -> Int
location mode i map
  | mode == 0 = map_lookup i map
  | mode == 1 = i

do_calc :: Int -> Int -> Int -> Int
do_calc a1 a2 t
  | t == 1 = a1 + a2
  | t == 2 = a1 * a2

map_lookup :: Int -> Map.Map Int Int -> Int
map_lookup n map = Maybe.fromMaybe 0 (Map.lookup n map)
