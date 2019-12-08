module Day8 where

import qualified Data.List as List
import Data.Char

type Image = [Int]
type Digits = [Int] -- length 25
type Layer = [[Int]] -- 25 * 6

process :: String -> IO ()
process image =
  let imageList = List.map Data.Char.digitToInt image
      layers = to_layers imageList
      decoded = decode_layer layers
  in putStrLn (to_string decoded)

-- part 2
decode_layer :: [Layer] -> Layer
decode_layer (x:xs) =
  List.foldl merge_layer x (x:xs)

merge_layer :: Layer -> Layer -> Layer
merge_layer layer1 layer2 =
  List.zipWith
   (\l1 l2 ->
       List.zipWith (\x1 x2 ->
                        case x1 of
                          2 -> x2
                          _ -> x1) l1 l2
   ) layer1 layer2

to_string :: Layer -> String
to_string layer =
  List.foldl
    (\acc l ->
       let str =
             List.foldl (\a x ->
                           let char = case x of
                                 1 -> "0"
                                 _ -> "_"
                           in a ++ char) "" l
       in acc ++ "\n" ++ str)
    "" layer
-- part 1
--      layer = find_layer_with_fewest_0 layers
--      ones = List.filter (\s -> s == 1) (List.concat layer)
--      twos = List.filter (\s -> s == 2) (List.concat layer)
--  in (length ones) * (length twos)

find_layer_with_fewest_0 :: [Layer] -> Layer
find_layer_with_fewest_0 layers =
  List.minimumBy compare_layer layers

compare_layer :: Layer -> Layer -> Ordering
compare_layer layer1 layer2 =
  let zeros1 = List.filter (\s -> s == 0) (List.concat layer1)
      zeros2 = List.filter (\s -> s == 0) (List.concat layer2)
  in compare zeros1 zeros2

to_layers :: Image -> [Layer]
to_layers image =
  let layerchunks = chunk_list (25*6) image []
  in List.map (\l -> chunk_list 25 l []) layerchunks

chunk_list :: Int -> [Int] -> [[Int]] -> [[Int]]
chunk_list size [] res = res
chunk_list size list res =
  let (head, tail) = List.splitAt size list
  in chunk_list size tail (res ++ [head])
