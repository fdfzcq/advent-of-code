module Day23 where

import Computer as C
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Debug.Trace

part1 :: [Int] -> Int
part1 input =
  let computerStates = init_computers [0..49] input
  in (process1 computerStates (M.fromList (L.map (\n -> (n, [])) [0..49])) (-1))

process1 :: [C.ComputerState] -> M.Map Int [Int] -> Int -> Int
process1 computerStates map yNat
  | isIdle =
    let cs0 = head computerStates
        (Just [xNat', yNat']) = M.lookup 255 map
    in case yNat' == yNat of
      True -> yNat
      False ->
        let computerStates' = (cs0 {inputs = [xNat', yNat']}:(tail computerStates))
            computerStates'' = process_cs computerStates'
            map' = M.delete 255 map
        in (process1 computerStates'' map' yNat')
  | otherwise =
    let map' = fill_queue_from_outputs computerStates map
        (computerStates', map'') = fill_inputs_from_queue computerStates map'
        computerStates'' = process_cs computerStates'
    in (process1 computerStates'' map'' yNat)
  where isIdle = check_idle map computerStates

check_idle :: M.Map Int [Int] -> [C.ComputerState] -> Bool
check_idle map computerStates =
  L.all
  (\(_, l) ->
      (l == []))
  (M.toList (M.delete 255 map)) &&
  (M.lookup 255 map /= Nothing) &&
  L.all
  (\cs -> outputs cs == [])
  computerStates

find_packet_to :: Int -> M.Map Int [Int] -> Int
find_packet_to n m
  | res == Nothing = (-1)
  | otherwise =
    let (Just list) = res
    in case list of
         [] -> (-1)
         _ -> head (tail list)
  where res = M.lookup n m

init_computers :: [Int] -> [Int] -> [C.ComputerState]
init_computers ids input =
  L.map
  (\n ->
     C.process_until_halt (C.initial_computer_state input [n]))
  ids

fill_queue_from_outputs :: [C.ComputerState] -> M.Map Int [Int] -> M.Map Int [Int]
fill_queue_from_outputs computerStates map =
  let (_, map') = L.foldl
                  (\(n, m) cs ->
                     let n' = n + 1
                         op = outputs cs
                         m' = case op == [] of
                                True -> m
                                False ->
                                  trace
                                  ("inputs " ++ show op)
                                  (append_inputs op m)
                     in (n', m'))
                  (0, map)
                  computerStates
  in map'

fill_inputs_from_queue :: [C.ComputerState] -> M.Map Int [Int] ->
  ([C.ComputerState], M.Map Int [Int])
fill_inputs_from_queue computerStates map =
  let (_, computerStates', map') =
        L.foldl
        (\(n, css, m) cs ->
            let n' = n + 1
                list = Maybe.fromMaybe [] (M.lookup n m)
                (css', m') = case list of
                              [] -> ( css ++ [cs {inputs = [-1, -1], outputs = []}]
                                    , m)
                              _ ->
                                let (x:y:list') = list
                                    input = [x, y]
                                in ( css ++ [cs {inputs = input, outputs = []}]
                                   , M.insert n list' m )
            in (n', css', m')
        )
        (0, [], map)
        computerStates
  in (computerStates', map')

process_cs :: [C.ComputerState] -> [C.ComputerState]
process_cs computerStates =
  L.map (\cs -> C.process_until_halt cs) computerStates

append_inputs :: [Int] -> M.Map Int [Int] -> M.Map Int [Int]
append_inputs [] m = m
append_inputs (k:x:y:xs) m =
  let old = Maybe.fromMaybe [] (M.lookup k m)
      new = case k of
              255 -> [x, y]
              _ -> old ++ [x, y]
      m' =  M.insert k new m
  in append_inputs xs m'
