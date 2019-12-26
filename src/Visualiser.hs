module Visualiser where

import qualified Data.Map.Strict as M
import qualified Data.List as List

visualise :: M.Map (Int, Int) Char -> IO ()
visualise gridMap = do
  let (xMin, yMin, xMax, yMax) = (find_border . M.keys) gridMap
  let grid = List.map (\a -> List.map (\b -> (b, a)) [xMin..xMax]) [yMin..yMax]
  putStrLn (plot grid gridMap)

plot :: [[(Int, Int)]] -> M.Map (Int, Int) Char -> String
plot grid gridMap =
  List.foldl (\acc line ->
                let str = List.foldl (\a p ->
                                        case M.lookup p gridMap of
                                          (Just c) -> a ++ [c]
                                          Nothing -> a ++ "_"
                                     ) "" line
                in acc ++ "\n" ++  str
             ) "" grid

find_border :: [(Int, Int)] -> (Int, Int, Int, Int)
find_border grid =
  let (xMin, _) = List.minimumBy (\(x1, y1) (x2, y2) -> compare x1 x2) grid
      (_, yMin) = List.minimumBy (\(x1, y1) (x2, y2) -> compare y1 y2) grid
      (xMax, _) = List.maximumBy (\(x1, y1) (x2, y2) -> compare x1 x2) grid
      (_, yMax) = List.maximumBy (\(x1, y1) (x2, y2) -> compare y1 y2) grid
  in (xMin, yMin, xMax, yMax)
