module Day17 where

import Computer

import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace

type Scaffolds = HashSet.HashSet (Int, Int)
type Intersections = [(Int, Int)]

data Robot = Robot { scaffolds :: Scaffolds
                   , facing :: Int
                   , robLoc :: (Int, Int)
                   , path :: [(String, Int)]} deriving Show

-- $# = 35  $. = 46 $\n = 10 $< = 60 $> = 62 $^ = 94 $v = 118
-- 0: up 1: right 2: down 3: left

-- part 2
-- part2 :: [Int] -> IO()
part2 :: [Int] -> [(String, Int)]
part2 program =
  let program' = (2:tail program)
      cs = Computer.initial_computer_state program' []
      cs' = Computer.process_until_halt cs
      camOutput = outputs cs'
      robot = Robot { scaffolds = HashSet.fromList []
                    , robLoc = (0, 0)
                    , facing = (-1)
                    , path = []}
      robot' = form_scaffolds (0, 0) camOutput robot
      -- visualisation
--      scaffs' = scaffolds robot'
--      tiles = Map.fromList (List.map (\x -> (x, 1)) (HashSet.toList scaffs'))
--      tiles' = Map.insert (robLoc robot') 0 tiles
--  in draw_tiles tiles'
  in path (find_path robot')

find_path :: Robot -> Robot
find_path robot
  | HashSet.size (scaffolds robot) == 0 =
    let p = path robot
    in robot {path = List.reverse p}
  | otherwise =
    let loc = robLoc robot
        scaffs = scaffolds robot
        face = facing robot
        p = path robot
        (nextLoc, face', turnCmd) =
          find_next_scaffold_and_facing face loc scaffs
        p' = case turnCmd of
               "N" ->
                 let (command, value) = head p
                 in ((command, value + 1):tail p)
               command ->
                 ((command, 1):p)
        scaffs' = case is_intersection nextLoc scaffs of
          True -> scaffs
          False ->
            HashSet.delete nextLoc scaffs
        robot' = robot { scaffolds = scaffs'
                       , facing = face'
                       , robLoc = nextLoc
                       , path = p'}
    in find_path robot'

find_next_scaffold_and_facing :: Int -> (Int, Int) -> Scaffolds ->
  ((Int, Int), Int, String)
find_next_scaffold_and_facing face loc scaffs =
  case is_intersection loc scaffs of
    True -> (next_location face loc, face, "N")
    False ->
      trace
      ("loc: " ++ (show loc) ++ (" scaffs: ") ++ (show (HashSet.toList scaffs)))
      (find_next_location face loc scaffs)

find_next_location :: Int -> (Int, Int) -> Scaffolds -> ((Int, Int), Int, String)
find_next_location face loc scaffs
  | HashSet.member (next_location face loc) scaffs =
      (next_location face loc, face, "N")
  | HashSet.member (next_location (turn_left face) loc) scaffs =
    (next_location (turn_left face) loc, turn_left face, "L")
  | HashSet.member (next_location (turn_right face) loc) scaffs =
    (next_location (turn_right face) loc, turn_right face, "R")

turn_left :: Int -> Int
turn_left 0 = 3
turn_left n = n - 1

turn_right :: Int -> Int
turn_right 3 = 0
turn_right n = n + 1

next_location :: Int -> (Int , Int) -> (Int, Int)
next_location 0 (x, y) = (x, y + 1)
next_location 1 (x, y) = (x + 1, y)
next_location 2 (x, y) = (x, y - 1)
next_location 3 (x, y) = (x - 1, y)

-- part 1
-- part1 :: [Int] -> Int
-- part1 program =
--   let initialCS = Computer.initial_computer_state program []
--       cs' = Computer.process_until_halt initialCS
--       camOutput = outputs cs'
--       state = Robot { scaffolds = HashSet.fromList []
--                     , curLoc = (0, 0)}
--       state' = form_scaffolds camOutput state
--       scaffs = scaffolds state'
--       intersections = find_intersections (HashSet.toList scaffs) scaffs
--   in calc_part1_res intersections

-- calc_part1_res :: [(Int, Int)] -> Int
-- calc_part1_res list =
--   List.sum (List.map (\(x, y) -> (abs x) * (abs y)) list)

find_intersections :: [(Int, Int)] -> Scaffolds -> [(Int, Int)]
find_intersections scaffList scaffolds =
  List.filter (\s -> is_intersection s scaffolds) scaffList

is_intersection :: (Int, Int) -> Scaffolds -> Bool
is_intersection (x, y) scaffolds =
  length (List.filter
          (\c ->
              HashSet.member c scaffolds)
          [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]) >= 3

form_scaffolds :: (Int, Int) -> [Int] -> Robot -> Robot
form_scaffolds coordinate [] state = state
form_scaffolds (x, y) (h:tl) state
  | h == 10 = form_scaffolds (0, y - 1) tl state
  | h == 46 = form_scaffolds (x + 1, y) tl state
  | h == 35 =
    let scaffs = scaffolds state
        scaffs' = HashSet.insert (x, y) scaffs
        state' = state { scaffolds = scaffs' }
    in form_scaffolds (x + 1, y) tl state'
  | (h == 60) || (h == 62) || (h == 94) || (h == 118) =
    let state' = state { facing = to_face(h)
                       , robLoc = (x, y)}
    in form_scaffolds (x + 1, y) tl state'
  | otherwise = state

to_face :: Int -> Int
to_face 94 = 0
to_face 62 = 1
to_face 118 = 2
to_face 60 = 3

--A,B,A,C,A,B,C,B,C,B

--A: ("L",4),("R",6),("R",10),("L",10)
--B: ("R",6),("L",12),("L",10)
--C: ("L",12),("L",4),("L",4),("R",10)

draw_tiles :: Map.Map (Int, Int) Int -> IO ()
draw_tiles tiles =
  let (xMin, yMin, xMax, yMax) = find_border (Map.keys tiles)
      game = List.map (\a -> List.map (\b ->  (b, a)) [xMin..xMax]) [yMin..yMax]
  in putStrLn (plot game tiles)

plot :: [[(Int, Int)]] -> Map.Map (Int, Int) Int -> String
plot game tiles =
  List.foldl (\acc line ->
                let str = List.foldl (\a p ->
                                        case Map.lookup p tiles of
                                          (Just 0) -> a ++ "@"
                                          (Just 1) -> a ++ "w"
                                          Nothing -> a ++ "_"
                                     ) "" line
                in acc ++ "\n" ++  str
             ) "" game

find_border :: [(Int, Int)] -> (Int, Int, Int, Int)
find_border panels =
  let (xMin, _) = List.minimumBy (\(x1, y1) (x2, y2) -> compare x1 x2) panels
      (_, yMin) = List.minimumBy (\(x1, y1) (x2, y2) -> compare y1 y2) panels
      (xMax, _) = List.maximumBy (\(x1, y1) (x2, y2) -> compare x1 x2) panels
      (_, yMax) = List.maximumBy (\(x1, y1) (x2, y2) -> compare y1 y2) panels
  in (xMin, yMin, xMax, yMax)

