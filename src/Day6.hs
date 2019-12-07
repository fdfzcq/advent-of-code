module Day6 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Split as List.Split

root = "COM"

-- part 2
part_2 :: [String] -> Int
part_2 xs =
  let edges = List.map splitStr xs
      map = do_process edges (Map.fromList [])
      pathToYou = find_path root "YOU" map []
      pathToSan = find_path root "SAN" map []
      intersection = pathToYou `List.intersect` pathToSan
  in length pathToYou + length pathToSan - (2 * length intersection)

find_path :: String -> String -> Map.Map String [String] -> [String] -> [String]
find_path cur obj map path
  | cur == obj = path
  | val == Nothing = []
  | otherwise =
    let (Just nodes) = val
    in find_right_path nodes obj map (cur:path)
  where val = Map.lookup cur map

find_right_path :: [String] ->  String ->
  Map.Map String [String] -> [String] -> [String]
find_right_path [] obj map path = []
find_right_path (x:xs) obj map path
  | p == [] =
      find_right_path xs obj map path
  | otherwise = p
  where p = find_path x obj map path


-- part 1
process :: [String] -> Int
process xs =
  let edges = List.map splitStr xs
      map = do_process edges (Map.fromList [])
  in total_orbits "COM" map 0

total_orbits :: String -> Map.Map String [String] -> Int -> Int
total_orbits node map orbits
  | value == Nothing = orbits
  | otherwise =
    let (Just tos) = value
        subOrbs = List.map (\x -> total_orbits x map (orbits + 1)) tos
    in (List.sum subOrbs) + orbits
  where value = Map.lookup node map

sum_orb :: Int -> Int -> Int
sum_orb 0 n = n
sum_orb i n = sum_orb (i - 1) (n + i)

splitStr :: String -> (String, String)
splitStr str =
  let [from, to] = List.Split.splitOn ")" str
  in (from, to)

do_process :: [(String, String)] -> Map.Map String [String] -> Map.Map String [String]
do_process [] map = map
do_process ((from, to):xs) map
  | value == Nothing =
    let newMap = Map.insert from [to] map
    in do_process xs newMap
  | otherwise =
    let (Just tos) = value
        newMap = Map.insert from (to:tos) map
    in do_process xs newMap
  where value = Map.lookup from map
