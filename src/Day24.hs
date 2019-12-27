module Day24 where

import qualified Data.Hashable as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Char as Char
import Debug.Trace

type GOL = M.Map (Int, Int, Int) Int
type History = S.Set Int

bug = Char.ord '#'
emptySpace = Char.ord '.'

-- part1 :: String -> Int
-- part1 input =
--   let gol = parse_input input
--   in process gol (S.fromList [])

part2 :: String -> Int
part2 input =
  let gol = parse_input input
  in process gol 200 0 0

process :: GOL -> Int -> Int -> Int -> Int
process gol n lMin lMax
  | n == 0 =
    (total_bugs (M.toList gol))
  | otherwise =
    let gol' = update_gol gol lMin lMax
    in process gol' (n - 1) (lMin - 1) (lMax + 1)

total_bugs :: [((Int, Int, Int), Int)] -> Int
total_bugs list =
  L.foldl
  (\n (c, v) ->
     case v == 35 of
       True ->
         (n + 1)
       False -> n)
  0
  list

-- process :: GOL -> History -> Int
-- process gol n
-- process gol history
--  | hasMatch = calculate_bio_diversity (M.toList gol) 0
--   | n == 0 = total_bugs gol
--   | otherwise =
--     let history' = add_to_history gol history
--         gol' = update_gol gol
--     in (process gol' history')
-- --  where hasMatch = has_match gol history

calculate_bio_diversity :: [((Int, Int), Int)] -> Int -> Int
calculate_bio_diversity [] n = n
calculate_bio_diversity (((x, y), v):xs) n
  | v == bug = calculate_bio_diversity xs (2 ^ (y * 5 + x) + n)
  | otherwise = calculate_bio_diversity xs n

update_gol :: GOL -> Int -> Int -> GOL
update_gol gol lMin lMax =
  let (gol', _) = L.foldl
                  update_bugs
                  (M.fromList [], gol)
                  ((M.toList gol) ++ (adjacent_levels lMin lMax))
  in gol'

adjacent_levels :: Int -> Int -> [((Int, Int, Int), Int)]
adjacent_levels lMin lMax =
  case lMin of
    0 -> gol_with_level 1 ++ gol_with_level (-1)
    _ -> gol_with_level (lMin - 1) ++ gol_with_level (lMax + 1)

gol_with_level :: Int -> [((Int, Int, Int), Int)]
gol_with_level l =
  (L.foldl (\g x -> g ++
                    L.foldl
                    (\g' y -> g' ++ [((x, y, l), 46)])
                    []
                    [0..4]
           ) [] [0..4]) L.\\ [((2, 2, l), 46)]

update_bugs :: (GOL, GOL) -> ((Int, Int, Int), Int) ->
  (GOL, GOL)
update_bugs (gol, gol0) (c, v)
  | (n == 1 || n == 2) && v == 46 =
    (M.insert c bug gol, gol0)
  | (n == 1) && v == 35 =
    (M.insert c bug gol, gol0)
  | otherwise =
      (M.insert c emptySpace gol, gol0)
  where n = count_bugs c gol0

count_bugs :: (Int, Int, Int) -> GOL -> Int
count_bugs (x, y, l) gol
  | otherwise =
   L.foldl
   (\n c ->
      case M.lookup c gol of
        (Just 35) -> (n + 1)
        _ -> n
   )
   0
   (adjacent_tiles (x, y, l))

adjacent_tiles :: (Int, Int, Int) -> [(Int, Int, Int)]
adjacent_tiles (x, y, l)
  | x == 2 && y == 1 =
    [(2, 0, l), (1, 1, l), (3, 1, l)] ++ L.map (\n -> (n, 0, l + 1)) [0..4]
  | x == 1 && y == 2 =
    [(1, 1, l), (0, 2, l), (1, 3, l)] ++ L.map (\n -> (0, n, l + 1)) [0..4]
  | x == 2 && y == 3 =
    [(1, 3, l), (2, 4, l), (3, 3, l)] ++ L.map (\n -> (n, 4, l + 1)) [0..4]
  | x == 3 && y == 2 =
    [(3, 1, l), (4, 2, l), (3, 3, l)] ++ L.map (\n -> (4, n, l + 1)) [0..4]
  | otherwise =
    let tilesX = case x of
                   0 -> [(1, 2, l - 1), (x + 1, y, l)]
                   4 -> [(3, 2, l - 1), (x - 1, y, l)]
                   _ -> [(x - 1, y, l), (x + 1, y, l)]
        tilesY = case y of
                   0 -> [(2, 1, l - 1), (x, y + 1, l)]
                   4 -> [(2, 3, l - 1), (x, y - 1, l)]
                   _ -> [(x, y - 1, l), (x, y + 1, l)]
    in tilesX ++ tilesY

add_to_history :: GOL -> History -> History
add_to_history gol history =
  let hs = H.hash (M.toList gol)
  in S.insert hs history

has_match :: GOL -> History -> Bool
has_match gol history =
  let hs = H.hash (M.toList gol)
  in S.member hs history

parse_input :: String -> GOL
parse_input input =
  let gol = M.fromList []
      lines = Split.splitOn ";" input
  in parse_lines lines gol 0

parse_lines :: [String] -> GOL -> Int -> GOL
parse_lines [] gol y = M.delete (2,2,0) gol
parse_lines (s:xs) gol y =
  let (_, gol') = L.foldl
                  (\(x, g) c ->
                     let g' = M.insert (x, y, 0) (Char.ord c) g
                     in (x + 1, g'))
                  (0, gol)
                  s
  in parse_lines xs gol' (y + 1)
