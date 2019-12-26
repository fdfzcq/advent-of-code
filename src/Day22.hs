module Day22 where

import qualified Data.List as L
import qualified Data.List.Split as S
import Debug.Trace

max_number = 119315717514046

base = 100000000

type Instruction = (Integer, Integer)

part1 :: String -> Integer -> Integer -> Integer
part1 input i times =
  let instructions = parse_input input
  in process instructions instructions i times

part2 :: Integer
part2 =
  process2 101741582076661 2020 base

process2 :: Integer -> Integer -> Integer -> Integer
process2 0 i b = i
process2 n i b
  | b == 100000000 && (n >= b) =
    let i' = (10311658519695 * i + 66224336603751) `rem` (max_number + 1)
    in trace
       ("processing n = " ++ show n ++ " i = " ++ show i')
       (process2 (n - b) i' b)
  | b == 10000 && (n >= b) =
    let i' = (11819823814140 * i + 51200051350268) `rem` (max_number + 1)
    in
      trace
      ("processing n = " ++ show n ++ " i = " ++ show i')
      (process2 (n - b) i' b)
  | b == 1 =
    let i' = (20660784091617 * i + 76404189563747) `rem` (max_number + 1)
    in
      trace
      ("processing n = " ++ show n ++ " i = " ++ show i')
      (process2 (n - b) i' b)
  | n < b =
    process2 n i (b `div` 10000)

calc :: Integer -> Integer -> Integer -> Integer
calc k 0 res = res + k
calc k n res =
  let res' = res + k * (20660784091617^n)
  in calc k (n - 1) res'

process :: [Instruction] -> [Instruction] -> Integer -> Integer -> Integer
process _ ins i 0 = i
process [] ins i t =
  trace
  ("t = " ++ show t ++ " i = " ++ show i)
  (process ins ins i (t - 1))
process (x:xs) ins i t =
  let i' = process_instruction x i
  in process xs ins i' t

process_instruction :: Instruction -> Integer -> Integer
process_instruction (ins, n) i
  | ins == 0 = deal_into_new_stack i
  | ins == 1 = cut n i
  | ins == 2 = deal_with_increment n i

deal_into_new_stack :: Integer -> Integer
deal_into_new_stack i = max_number - i

cut :: Integer -> Integer -> Integer
cut n i
  | n > 0 =
    case i >= max_number - n + 1 of
      True -> i - (max_number - n + 1)
      False -> i + n
  | otherwise =
    cut (max_number + n + 1) i

deal_with_increment :: Integer -> Integer -> Integer
deal_with_increment n i' =
  find_i_increment n i' 0

find_i_increment :: Integer -> Integer -> Integer -> Integer
find_i_increment n i' m
--  | i <= max_number = i
--  | i > max_number = find_i_increment n i' (m + 1)
  | x `rem` n /= 0 = find_i_increment n i' (m + 1)
  | otherwise = x `div` n
  where x = i' + m * (max_number + 1)
--deal_with_increment n i = n * i `rem` (max_number + 1)

rem2 :: Integer -> Integer -> Integer
rem2 a b
  | a `rem` b == 0 = b
  | otherwise = a `rem` b

parse_input :: String -> [Instruction]
parse_input input =
  let lines = S.splitOn ";" input
  in convert_lines_to_instructions lines []

convert_lines_to_instructions :: [String] -> [Instruction] -> [Instruction]
convert_lines_to_instructions [] instructions = instructions
convert_lines_to_instructions (x:xs) instructions
  | x == "deal into new stack" = convert_lines_to_instructions xs ((0, 0):instructions)
  | "cut" `L.isPrefixOf` x =
      let i = read (x L.\\ "cut ") :: Integer
      in convert_lines_to_instructions xs ((1, i):instructions)
  | "deal with increment" `L.isPrefixOf` x =
      let i = read (x L.\\ "deal with increment ") :: Integer
      in convert_lines_to_instructions xs ((2, i):instructions)
