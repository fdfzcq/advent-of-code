module Day25 where

import Computer
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.List.Split as Split
import Debug.Trace

command = "west\nsouth\ntake planetoid\nnorth\nwest\ntake festive hat\neast\neast\nnorth\nwest\neast\nnorth\ntake sand\nsouth\nsouth\nsouth\ntake space heater\nwest\ntake wreath\nsouth\ntake space law space brochure\nsouth\ntake pointer\nnorth\nnorth\neast\nsouth\neast\ntake loom\nwest\nnorth\nnorth\nwest\nwest\nsouth\nwest\n"

items = [ "planetoid", "festive hat", "space heater", "loom", "space law space brochure"
        , "sand", "pointer", "wreath" ]

part1 :: [Int] -> [Computer.ComputerState]
part1 input =
  let cs = Computer.initial_computer_state input []
  in process1 (Computer.process_until_halt cs)

play :: Computer.ComputerState -> String -> [Int]
play cs command =
  let cs' = cs {inputs = str_to_int_list command ++ [10], outputs = []}
      cs'' = Computer.process_until_halt cs'
  in trace
     ("outputs: " ++ show (L.map (\i -> C.chr i) (outputs cs'')))
     inputs cs''

process1 :: Computer.ComputerState -> [Computer.ComputerState]
process1 cs =
  let cs' = Computer.process_until_halt (cs { inputs = str_to_int_list command
                                            , outputs = []})
      commands = L.map
                 (\set ->
                    let str = L.foldl
                              (\acc s -> acc ++ "drop " ++ s ++ "\n")
                              ""
                              set
                    in trace
                       (show str)
                       (str_to_int_list (str ++ "north\n")))
                 (subset items)
      computerStates = L.map
                       (\c ->
                           Computer.process_until_halt (cs' { inputs = c
                                                            , outputs = []}))
                       commands
  in L.filter (\c -> should_continue (outputs c)) computerStates

should_continue :: [Int] -> Bool
should_continue [] = False
should_continue list =
  let lines = Split.splitOn [10] list
  in trace
     ("outputs: " ++ show (L.map (\i -> C.chr i) list))
     (lines L.!! (length lines - 2) /= str_to_int_list "Command?")

subset :: [String] -> [[String]]
subset [] = [[]]
subset (x:xs) = [zs | ys <- subset xs, zs <- [ys,(x:ys)]]

get_commands :: [Int] -> [[Int]]
get_commands outputs =
  let lines = Split.splitOn [10] outputs
      options = L.filter (\l -> case l of
                                  [] -> False
                                  _ -> head l == C.ord '-'
                                    && l /= str_to_int_list "- infinite loop"
                         ) lines
      options' = case options of
                   [] -> L.map str_to_int_list ["east", "west", "south", "north"]
                   _ -> options
  in L.map (\l -> to_command (l L.\\ [C.ord '-', C.ord ' '])) options'

to_command :: [Int] -> [Int]
to_command item
  | item /= str_to_int_list "east" && item /= str_to_int_list "west" &&
    item /= str_to_int_list "south" && item /= str_to_int_list "north" =
    str_to_int_list "take " ++ item
  | otherwise =
    item

str_to_int_list :: [Char] -> [Int]
str_to_int_list str =
  L.map (\c -> C.ord c) str
