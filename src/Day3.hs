module Day3 where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.List as List

-- part 2
part_2 :: ([String], [String]) -> Int
part_2 (wire1, wire2) =
  let list1 = reverse (get_wire_list wire1)
      list2 = get_wire_list wire2
      map2 = Map.fromList (zip (reverse list2) [1..])
      max0 = (length list1) + (length list2)
  in find_min2 list1 map2 1 max0

find_min2 :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Int -> Int -> Int
find_min2 [] map n m = m
find_min2 (x:xs) map n m =
  case Map.lookup x map of
    Nothing ->
      find_min2 xs map (n + 1) m
    (Just index2) ->
      let newMin = min (n + index2) m
      in find_min2 xs map (n + 1) newMin

-- part 1
part_1 :: ([String], [String]) -> Int
part_1 (wire1, wire2) =
  let list1 = get_wire_list wire1
      list2 = get_wire_list wire2
  in find_min (List.sortBy sortLT list1) list2

find_min :: [(Int, Int)] -> [(Int, Int)] -> Int
find_min (x:xs) list =
  case elem x list of
    True -> m_distance x
    _ -> find_min xs list

sortLT :: (Int, Int) -> (Int, Int) -> Ordering
sortLT a b =
  compare (m_distance a) (m_distance b)

get_wire_list :: [String] -> [(Int, Int)]
get_wire_list xs =
  get_wire_list2 xs (0,0) []

get_wire_list2 :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
get_wire_list2 [] location list = list
get_wire_list2 (x:xs) location list =
  let direction = head x
      length = read (tail x) :: Int
      newList = add_locations_to_list direction length location list
  in get_wire_list2 xs (head newList) newList

add_locations_to_list :: Char -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
add_locations_to_list dir 0 location list = list
add_locations_to_list dir length location list =
  let newLocation = get_new_location dir location
      newList = (newLocation : list)
  in add_locations_to_list dir (length - 1) newLocation newList

get_new_location :: Char -> (Int, Int) -> (Int, Int)
get_new_location direction (x, y)
  | direction == 'R' = (x + 1, y)
  | direction == 'L' = (x - 1, y)
  | direction == 'U' = (x, y + 1)
  | direction == 'D' = (x, y - 1)

m_distance :: (Int, Int) -> Int
m_distance (x, y) =
  abs(x) + abs(y)
