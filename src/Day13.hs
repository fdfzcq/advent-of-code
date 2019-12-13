module Day13 where

import Computer hiding (process)
import qualified Data.List as List
import qualified Data.Map as Map

type Tiles = Map.Map (Int, Int) Int

process :: [Int] -> IO Char
process program =
  let initialCS = Computer.initial_computer_state program []
  in do_play initialCS (Map.fromList []) 0

do_play :: Computer.ComputerState -> Tiles -> Int -> IO Char
do_play cs ts score =
  do
    let cs' = process_until_halt cs
    let tiles = to_tiles (outputs cs') ts
    let score' = get_score (outputs cs') score
    draw_tiles tiles
    putStrLn ("current score" ++ (show score'))
    putStrLn ("please give instruction")
    input <- getChar
    let ips = case input of
          'q' -> [-1]
          'e' -> [1]
          'w' -> [0]
          _ -> []
    let cs'' = cs' {inputs = ips, outputs = []}
    do_play cs'' tiles score'

get_score :: [Int] -> Int -> Int
get_score [] s = s
get_score (-1:0:score:xs) s = score
get_score (x:y:t:xs) s = get_score xs s

find_by_tile_id :: Int -> [Int] -> (Int, Int, Int)
find_by_tile_id id (x:y:id':xs)
  | id == id' = (x, y, id')
  | otherwise = find_by_tile_id id xs

to_tiles :: [Int] -> Tiles -> Tiles
to_tiles [] tiles = tiles
to_tiles (-1:0:_:xs) tiles = to_tiles xs tiles
to_tiles (xValue:yValue:tId:xs) tiles =
  let tiles' = Map.insert (xValue, yValue) tId tiles
  in to_tiles xs tiles'

block_number :: Tiles -> Int
block_number tiles =
  length (List.filter (\((_, _),x) -> x == 2) (Map.toList tiles))

draw_tiles :: Tiles -> IO ()
draw_tiles tiles =
  let (xMin, yMin, xMax, yMax) = find_border (Map.keys tiles)
      game = List.map (\a -> List.map (\b ->  (b, a)) [xMin..xMax]) [yMin..yMax]
  in putStrLn (plot game tiles)

plot :: [[(Int, Int)]] -> Tiles -> String
plot game tiles =
  List.foldl (\acc line ->
                let str = List.foldl (\a p ->
                                        case Map.lookup p tiles of
                                          (Just 0) -> a ++ "_"
                                          (Just 1) -> a ++ "w"
                                          (Just 2) -> a ++ "b"
                                          (Just 3) -> a ++ "p"
                                          (Just 4) -> a ++ "*"
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

