module Day10 where

import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Ratio as Ratio

type Asteroid = (Int, Int)
type Asteroids = [Asteroid]
type Formula = (Coefficient, Constant, Direction)
type Coefficient = (Int, Int)
type Constant = (Int, Int)
type Direction = Bool

monitorStation = (8, 16)

-- process :: String -> Int
process :: String -> Int -> Asteroid
process input i =
  let asteroids = parse_input input
  -- part 1 (_, max) = find_max_detectable asteroids
  in find_nth_destroyed i asteroids
--  in find_detectable 200 monitorStation asteroids []

find_nth_destroyed :: Int -> Asteroids -> Asteroid
find_nth_destroyed n asteroids
  | length(asts) >= n = (List.sortBy sort_asteroids asts)!!(n - 1)
  | otherwise =
      let newN = n - length(asts)
          newAsts = asteroids List.\\ asts
      in find_nth_destroyed newN newAsts
  where (formulas, asts) = find_detectable monitorStation asteroids ([], [])

sort_asteroids :: Asteroid -> Asteroid -> Ordering
sort_asteroids asteroid1 asteroid2 =
  compare (to_rad asteroid1 monitorStation) (to_rad asteroid2 monitorStation)

get_dist :: Asteroid -> Asteroid -> Int
get_dist (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

to_rad :: Asteroid -> Asteroid -> Float
to_rad (x1, y1) (x2, y2)
  | x1 == x2 =
    case y1 < y2 of
      True -> 0.0
      False -> pi
  | otherwise =
    let angle = atan (fromIntegral (y1 - y2)/fromIntegral (x1 - x2))
    in case x1 > x2 of
      True -> pi/2 + angle
      False -> (1.5 * pi) + angle

parse_input :: String -> Asteroids
parse_input input =
  let lines = Split.splitOn ";" input
  in List.concat (List.foldl parse_line [] lines)

parse_line :: [Asteroids] -> String -> [Asteroids]
parse_line xs line =
  let y = length xs
      (asteroids, _) =
        List.foldl (\acc x -> find_asteroid acc x y) ([], 0) line
  in (asteroids:xs)

find_asteroid :: (Asteroids, Int) -> Char -> Int -> (Asteroids, Int)
find_asteroid (xs, x) '.' y = (xs, x + 1)
find_asteroid (xs, x) '#' y = ((x, y):xs, x + 1)

find_max_detectable :: Asteroids -> (Asteroid, Int)
find_max_detectable asteroids =
  List.foldl (\max x ->
                count_and_compare_detectable max x asteroids
             ) ((0,0), 0) asteroids

count_and_compare_detectable :: (Asteroid, Int) -> Asteroid -> Asteroids -> (Asteroid, Int)
count_and_compare_detectable (ast, max) asteroid asteroids
  | (length detectable_asteroids) > max = (asteroid, length detectable_asteroids)
  | otherwise = (ast, max)
  where (detectable_asteroids, _) = find_detectable asteroid asteroids ([], [])

find_detectable :: Asteroid -> Asteroids -> ([Formula], Asteroids) -> ([Formula], Asteroids)
find_detectable asteroid [] formulaAsts = formulaAsts
find_detectable asteroid (x:xs) formulaAsts
  |asteroid == x = find_detectable asteroid xs formulaAsts
  |otherwise =
     let formula = get_formula asteroid x
         (formulas, asts) = formulaAsts
     in case List.elem formula formulas of
          True -> find_detectable asteroid xs formulaAsts
          False -> find_detectable asteroid xs ((formula:formulas), (x:asts))

-- modified in part 2
get_formula :: Asteroid -> Asteroid -> Formula
get_formula (x1, y1) (x2, y2)
  | y1 == y2 = ((0, 0), (0, 0), x1 > x2)
  | otherwise =
    let coefRatio = (x1 - x2) Ratio.% (y1 - y2)
        coefficient = (Ratio.numerator coefRatio, Ratio.denominator coefRatio)
        constRatio = (y1 Ratio.% 1) - (coefRatio * (x1 Ratio.% 1))
        constant = (Ratio.numerator constRatio, Ratio.denominator constRatio)
        direction = case (x1 == x2) of
                      True -> y1 > y2
                      False -> x1 > x2
    in (coefficient, constant, direction)
