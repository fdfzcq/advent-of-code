module Day14 where

import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Data.HashSet as HashSet
import qualified Data.Hashable as Hashable

type Rt = (Int, Int)
type Component = (Int, Chemical)
type Formulas = Map.Map Chemical (Int, [Component])
type Chemical = String
type Wastes = Map.Map Chemical Int

data State = State { formulas :: Formulas
                   , ore :: Int
                   , wastes :: Wastes }

cargo = 1000000000000

process :: String -> IO ()
process input =
  let forms  = parse_input input
      state = State { formulas = forms
                    , wastes = Map.fromList []
                    , ore = 0}
  in calculate_req 10000 0 state

calculate_req :: Int -> Int -> State -> IO ()
calculate_req base n state
  | (ore state) >= cargo =
    do putStrLn ("res" ++ show (n - 1))
  | (((ore state) + (base * 198984)) >= cargo) && (base /= 1) =
    do calculate_req 1 n state
  | otherwise =
    do
      putStrLn ("ore: " ++ (show (ore state)))
      putStrLn ("n: " ++ (show n))
      let (Just (_, components)) = Map.lookup "FUEL" (formulas state)
      let state' = do_process base components state
      calculate_req base (n + base) state'

total_waste :: Wastes -> Int
total_waste wastes =
  let list = Map.toList wastes
  in List.foldl (\acc (_, v) -> acc + v) 0 list

do_process :: Int -> [Component] -> State -> State
do_process 0 l state = state
do_process n [] state = state
do_process reqN ((n, "ORE"):xs) state =
  let oreN = ore state
  in do_process reqN xs (state {ore = oreN + n * reqN})
do_process reqN ((requiredChemN, chem):xs) state =
  let wastes0 = wastes state
      wasteN = Maybe.fromMaybe 0 (Map.lookup chem wastes0)
      requiredChemN' = reqN * requiredChemN
      (Just (smallestCompN, componentsChem)) = Map.lookup chem (formulas state)
      -- calculate waste
      (reqN', wasteN') = calc_waste requiredChemN' wasteN smallestCompN
      wastes' = Map.insert chem wasteN' wastes0
      state' = state { wastes = wastes' }
      state'' = do_process reqN' componentsChem state'
  in do_process reqN xs state''

calc_waste :: Int -> Int -> Int -> (Int, Int)
calc_waste requiredChemical wasteN smallestUnit
  | requiredChemical <= wasteN = (0, wasteN - requiredChemical)
  | otherwise =
    let requiredChemical' = requiredChemical - wasteN
        requiredTimes = ceiling(fromIntegral requiredChemical'/fromIntegral smallestUnit)
        wasteN' = requiredTimes * smallestUnit - requiredChemical'
    in (requiredTimes, wasteN')

parse_input :: String -> Formulas
parse_input input =
  let lines = Split.splitOn ";" input
  in to_formulas lines (Map.fromList [])

to_formulas :: [String] -> Formulas -> Formulas
to_formulas [] formulas = formulas
to_formulas (formStr:xs) formulas =
  let (chem, components) = parse_formula formStr
      newFormulas = Map.insert chem components formulas
  in to_formulas xs newFormulas

parse_formula :: String -> (Chemical, (Int, [Component]))
parse_formula formStr =
  let [compoStr, chemStr] = Split.splitOn " => " formStr
      [chemN0, chemical] = Split.splitOn " " chemStr
      chemN = read chemN0 :: Int
      componentStrs = Split.splitOn ", " compoStr
      components = List.map (\s ->
                               let [n0, component] = Split.splitOn " " s
                                   n = read n0 :: Int
                               in (n, component)) componentStrs
  in (chemical, (chemN, components))
