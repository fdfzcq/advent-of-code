module Day2 where

import Prelude hiding (lookup)
import qualified Data.Map as Map

-- part 2
part_2 :: [Int] -> Int
part_2 xs =
  let noun0 = 99
      verb0 = 99
      (noun, verb) = find_val noun0 verb0 xs
  in 100 * noun + verb

find_val :: Int -> Int -> [Int] -> (Int, Int)
find_val noun verb xs
  | output == 19690720 = (noun, verb)
  | output < 19690720 = do find_val 99 (verb - 1) xs
  | output > 19690720 = do find_val (noun - 1) verb xs
  where output = process_list noun verb xs

process_list :: Int -> Int -> [Int] -> Int
process_list noun verb xs =
  let zippedList = zip [0..] xs
      map0 = Map.fromList zippedList
      map = Map.insert 1 noun (Map.insert 2 verb map0)
  in do_part_1 map 0

-- part 1
process1 :: [Int] -> Int
process1 xs =
  process_list 12 2 xs

do_part_1 :: Map.Map Int Int -> Int -> Int
do_part_1 map n =
  case map_lookup n map of
    99 -> do
      map_lookup 0 map
    t ->
      let i1 = map_lookup (n + 1) map
          a1 = map_lookup i1 map
          i2 = map_lookup (n + 2) map
          a2 = map_lookup i2 map
          i = map_lookup (n + 3) map
          a = do_calc a1 a2 t
      in do_part_1 (Map.insert i a map) (n + 4)

map_lookup :: Int -> Map.Map Int Int -> Int
map_lookup n map = Maybe.fromMaybe 0 (Map.lookup n map)

do_calc :: Int -> Int -> Int -> Int
do_calc a1 a2 t
  | t == 1 = a1 + a2
  | t == 2 = a1 * a2

-- let a = b in c             (\a -> c) b
-- do a <- b; c               (b >>= \a -> c)

-- interface Monad<M<_>> {
--   <A, B> bind(this: M<A>, fn: (A) -> M<B>): M<B>,
--   <A> return(value: A): M<A>
--                       }

-- class Monad<Maybe<_>> {
--   bind(this: Maybe<A>, fn: (A) -> Maybe<B>): Maybe<B>
--                       }

-- class Monad {
--   bind(this: Monad<A>, fn: (A) -> Monad<B>): Monad<B>
--                }

-- Maybe(1).bind(x -> [1])

-- type positive_int = { i:int | i >= 0 }
-- type sameTuple = { (i1:int, i2:int) | i1 == i2 }
