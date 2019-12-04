module Day4 where

import qualified Data.List as List

minVal = 134792
maxVal = 675810

-- part 1
process :: Int
process =
  let list0 = [minVal..maxVal]
  in length (find_passwords list0)

find_passwords :: [Int] -> [Int]
find_passwords list =
  filter find_password_fun list

find_password_fun :: Int -> Bool
find_password_fun n =
  let digits = to_digits_list n []
  in is_password digits

to_digits_list :: Int -> [Int] -> [Int]
to_digits_list n digits
  | n < 10 = (n:digits)
  | otherwise =
    let newN = n `div` 10
        digit = n `rem` 10
    in to_digits_list newN (digit:digits)

is_password :: [Int] -> Bool
is_password digits =
  let sorted = List.sort digits
      hasDupDigits = has_adjacent_dup_digits digits
  in (sorted == digits) && hasDupDigits

has_adjacent_dup_digits :: [Int] -> Bool
has_adjacent_dup_digits [] = False
has_adjacent_dup_digits [_] = False
has_adjacent_dup_digits (x1:x2:xs)
  -- part 1
  -- | x1 == x2 = True
  -- part 2
  | (x1 == x2) && (xs == []) = True
  | (x1 == x2) && ((head xs) == x1) =
      has_adjacent_dup_digits (unmatched_tail xs)
  | (x1 == x2) = True
  | otherwise =
    has_adjacent_dup_digits (x2:xs)

unmatched_tail :: [Int] -> [Int]
unmatched_tail [_] = []
unmatched_tail (x:xs)
  | x == (head xs) = unmatched_tail xs
  | otherwise = xs
