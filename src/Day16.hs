module Day16 where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Char as Char
import Debug.Trace

type Signal = [Int]
type Pattern = [Int]

data State = State { index :: Int
                   , phase :: Int
                   , mem :: Map.Map Int Int
                   , len :: Int
                   , digit :: Int} deriving Show

process :: String -> Int -> String
process input phase =
  let initSignal = parse_input input
      signal = fft initSignal 0 phase
  in List.foldl (\acc i -> acc ++ (show i)) "" signal

-- part 2 --
part2 :: String -> Int -> String
part2 input off =
  let sig = parse_input input
      state = State { index = (length sig) * 1000 - 1
                    , phase = 100
                    , mem = init_mem sig 0 (Map.fromList [])
                    , len = (length sig) * 1000
                    , digit = (-1)}
      digits = get_digits state [] off
  in List.foldl (\acc i -> acc ++ (show i)) "" digits

init_mem :: Signal -> Int -> Map.Map Int Int -> Map.Map Int Int
init_mem [] i m = m
init_mem (x:xs) i m =
  let m' = Map.insert (calc_map_key 0 i) x m
  in init_mem xs (i + 1) m'

calc_map_key :: Int -> Int -> Int
calc_map_key phase index =
  phase + (index * 1000)

get_digits :: State -> [Int] -> Int -> [Int]
get_digits state res off
  | index state < off = res
  | otherwise =
    let i = index state
        state' = calculate_digit state
        dig = digit state'
        res' = case i < (off + 8) of
                 True -> (dig:res)
                 False -> res
    in (get_digits (state' {index = i - 1}) res' off)
      --trace
      --("digit: " ++ (show dig) ++ " i: " ++ (show i))

calculate_digit :: State -> State
calculate_digit state
  | maybeDigit == Nothing =
    trace
    ("i: " ++ (show (index state)) ++ " p: " ++ (show (phase state)))
    (calculate_digit_and_update_mem state)
  | otherwise =
    let (Just dig) = maybeDigit
    in state { digit = dig }
  where maybeDigit = Map.lookup (calc_map_key (phase state) (index state)) (mem state)

calculate_digit_and_update_mem :: State -> State
calculate_digit_and_update_mem state
  | phase state == 0 =
    let i = index state
        p = phase state
        state' = calculate_digit (state { index = i `rem` ((len state) `div` 1000)})
        dig = digit state'
    in state {digit = dig}
  | (index state) == (len state) - 1 =
    let i = index state
        p = phase state
        state' = calculate_digit (state { index = ((len state) `div` 1000) - 1
                                        , phase = 0})
        dig = digit state'
    in state {digit = dig}
  | otherwise =
    let i = index state
        p = phase state
        state' = calculate_digit (state { index = i
                                        , phase = p - 1})
        dig1 = digit state'
        state'' = calculate_digit (state' { index = i + 1
                                          , phase = p})
        dig2 = digit state''
        memMap = mem state''
        dig = abs (dig1 + dig2) `rem` 10
        memMap' = Map.insert (calc_map_key p i) dig memMap
        memMap'' = Map.delete (calc_map_key p (i + 101)) memMap'
    in state'' { mem = memMap''
               , digit = dig}

parse_input :: String -> Signal
parse_input str =
  List.map Char.digitToInt str

fft :: Signal -> Int -> Int -> Signal
fft sig phase finalPhase
  | phase == finalPhase = sig
  | otherwise =
    let sig' = do_phase sig 1 []
    in fft sig' (phase + 1) finalPhase

do_phase :: Signal -> Int -> Signal -> Signal
do_phase sig i res
  | i > (length sig) = List.reverse res
  | otherwise =
      let digit = calc_digit i sig 0 0
      in do_phase sig (i + 1) (digit:res)

calc_digit :: Int -> Signal -> Int -> Int -> Int
calc_digit i [] index res = (abs res) `rem` 10
calc_digit i (x:xs) index res =
  let res' = res + calc_val i (index, x)
  in calc_digit i xs (index + 1) res'

calc_val :: Int -> (Int, Int) -> Int
calc_val i (valIndex, val) =
  let index = floor (fromIntegral (valIndex + 1)/fromIntegral i)
      index' = index `rem` 4
  in (get_val index') * val

get_val :: Int -> Int
get_val 0 = 0
get_val 1 = 1
get_val 2 = 0
get_val 3 = (-1)
