module Day15 where

import Computer
import qualified Data.Map as Map
import qualified Data.List as List

type Coordinate = (Int, Int)

data Droid = Droid { curLoc :: Coordinate
                   , computerState :: Computer.ComputerState
                   , labrinth :: Map.Map Coordinate Int
                   , moves :: Int} deriving Show

oxygenLocation = (-20,14)

process :: [Int] -> Int
process program =
  let initialCS = Computer.initial_computer_state program []
      initialDroid = Droid { curLoc = (0, 0)
                           , computerState = initialCS {outputs = [1]}
                           , labrinth = Map.fromList [((0, 0), 3)]
                           , moves = 0}
      droid = draw_map initialDroid
  in fill_oxygen oxygenLocation (labrinth droid) 0 []

fill_oxygen :: Coordinate -> Map.Map Coordinate Int -> Int -> [Coordinate] -> Int
fill_oxygen currentLocation lab time path =
  let allPossibleMoves = List.map (\m -> move currentLocation m) [1..4]
      path' = (currentLocation:path)
      possibleMoves = List.filter
                      (\(c, _) ->
                          let val = Map.lookup c lab
                              notPassed = List.notElem c path
                          in (val /= Nothing) && (val /= (Just 0)) && notPassed
                      ) allPossibleMoves
      possibleMoves' = List.map (\(c, _) -> c) possibleMoves
      time' = time + 1
  in case possibleMoves of
       [] -> time
       _ ->
         List.maximumBy compare (List.map (\c ->
                                             fill_oxygen c lab time' path'
                                           ) possibleMoves')

draw_map :: Droid -> Droid
draw_map droid
  | outputs processedCS == [0] =
      let lab = labrinth droid
          lab' = Map.insert (curLoc droid) 0 lab
      in droid { labrinth = lab'}
  | otherwise =
    let [res] = outputs processedCS
        lab = labrinth droid
        lab' = Map.insert (curLoc droid) res lab
        droid'' = droid { labrinth = lab'
                        , computerState = processedCS }
    in case get_possible_moves droid of
         [] -> droid''
         nextCoorAndMoves ->
           List.foldl
           (\d x ->
               let d' = do_next_move d x
                   l' = labrinth d'
               in d {labrinth = l'}
           ) droid'' nextCoorAndMoves
  where processedCS = Computer.process_until_halt (computerState droid)

detect :: Droid -> Droid
detect droid
  | outputs processedCS == [2] =
    let lab = labrinth droid
        lab' = Map.insert (curLoc droid) 2 lab
    in droid {labrinth = lab'}
  | outputs processedCS == [1] =
    case get_possible_moves droid of
      [] -> droid {moves = (-1)}
      nextCoorAndMoves ->
        let lab = labrinth droid
            lab' = Map.insert (curLoc droid) 1 lab
            droid' = droid { labrinth = lab'
                           , computerState = processedCS }
            droids = List.map (\x -> do_next_move droid' x) nextCoorAndMoves
        in case (List.filter (\d -> (moves d ) /= (-1)) droids) of
          [] -> droid' {moves = (-1)}
          filtered ->
            List.minimumBy (\d1 d2 -> compare (moves d1) (moves d2)) filtered
  | otherwise = droid {moves = (-1)}
  where processedCS = Computer.process_until_halt (computerState droid)

do_next_move :: Droid -> (Coordinate, Int) -> Droid
do_next_move prevDroid (nextLoc, move) =
  let cs = computerState prevDroid
      mvs = moves prevDroid
      cs' = cs { inputs = [move]
               , outputs = []}
      droid = prevDroid { computerState = cs'
                        , curLoc = nextLoc
                        , moves = mvs + 1 }
  in draw_map droid

get_possible_moves :: Droid -> [(Coordinate, Int)]
get_possible_moves droid =
  let loc = curLoc droid
      lab = labrinth droid
      allPossibleMoves = List.map (\m -> move loc m) [1..4]
  in List.filter (\(c, m) -> Map.lookup c lab == Nothing) allPossibleMoves

move :: Coordinate -> Int -> (Coordinate, Int)
move (x, y) m
  | m == 1 = ((x, y + 1), m)
  | m == 2 = ((x, y - 1), m)
  | m == 3 = ((x - 1, y), m)
  | m == 4 = ((x + 1, y), m)
