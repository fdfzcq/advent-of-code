module Day1 where

get_sum :: [Int] -> Int
get_sum [] = 0
get_sum (x:xs) =
  total_fuel_2 x + get_sum xs

-- part 1
total_fuel_1 :: Int -> Int
total_fuel_1 n = n `div` 3 - 2

-- part 2
total_fuel_2 :: Int -> Int
total_fuel_2 n
  -- total_fuel_1 n <= 0
  | n <= 6 = 0
  | otherwise = total_fuel_1 n + total_fuel_2 (total_fuel_1 n)
