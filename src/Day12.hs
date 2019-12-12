module Day12 where

import qualified Data.List as List
import qualified Data.Hashable as Hashable
import qualified Data.HashSet as HashSet
import qualified Data.Ratio as Ratio

data Moon = Moon
  { position :: Position
  , velocity :: Velocity
  } deriving Show
type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
type History = HashSet.HashSet Int

input = [ (1,4,4), ((-4), (-1), 19), ((-15), (-14), 12), ((-17), 1, 10)]

-- input = [ (-8, -10, 0)
--         , (5, 5, 10)
--         , (2, -7, 3)
--         , (9, -8, -3)]

-- input = [ (-1, 0, 2)
--         , (2, -10, -7)
--         , (4, -8, 8)
--         , (3, 5, -1)]

process :: Int
process =
  let moons =
        List.map (\c ->
                    Moon { position = c
                         , velocity = (0, 0, 0) }) input
--  in step steps moons
      resX = steps_until_hist_match 0 moons 0 (HashSet.fromList [])
      resY = steps_until_hist_match 1 moons 0 (HashSet.fromList [])
      resZ = steps_until_hist_match 2 moons 0 (HashSet.fromList [])
  in calc_res (calc_res resX resY) resZ
--  in total_energy moons'

calc_res :: Int -> Int -> Int
calc_res x y =
  x * y `div` (x `div` Ratio.numerator (x Ratio.% y))

total_energy :: [Moon] -> Int
total_energy moons =
  let moonsEnergy = List.map calc_moon_energy moons
  in List.sum moonsEnergy

calc_moon_energy :: Moon -> Int
calc_moon_energy moon =
  let (xp, yp, zp) = position moon
      pot = (abs xp) + (abs yp) + (abs zp)
      (xv, yv, zv) = velocity moon
      kin = (abs xv) + (abs yv) + (abs zv)
  in pot * kin

-- part 1 --
step :: Int -> [Moon] -> [Moon]
step 0 moons = moons
step n moons =
  let moons' = update_moons moons
  in step (n - 1) moons'

--
steps_until_hist_match :: Int -> [Moon] -> Int -> History -> Int
steps_until_hist_match option moons n xs
  | HashSet.member hs xs = n
  | otherwise =
    let moons' = update_moons moons
    in steps_until_hist_match option moons' (n + 1) (HashSet.insert hs xs)
  where hs = do_hash moons option

do_hash :: [Moon] -> Int -> Int
do_hash moons option =
  let list = List.map (\m -> hash_moon option m) moons
  in Hashable.hash list

hash_moon :: Int -> Moon -> (Int, Int)
hash_moon option moon =
  (get_val option (position moon), get_val option (velocity moon))

get_val :: Int -> (Int, Int, Int) -> Int
get_val n (x, y, z)
  | n == 0 = x
  | n == 1 = y
  | n == 2 = z

update_moons :: [Moon] -> [Moon]
update_moons moons =
  let moons' = List.map (\m -> update_velocity m moons) moons
  in List.map update_position moons'

update_position :: Moon -> Moon
update_position moon =
  let (xp, yp, zp) = position moon
      (xv, yv, zv) = velocity moon
      pos = (xp + xv, yp + yv, zp + zv)
  in moon {position = pos}

update_velocity :: Moon -> [Moon] -> Moon
update_velocity moon moons =
  let velo = velocity moon
      velo' = List.foldl (\v m ->
                            calc_velocity v (position moon) (position m)
                         ) velo moons
  in moon {velocity = velo'}

calc_velocity :: Velocity -> Position -> Position -> Velocity
calc_velocity (x, y, z) (xp, yp, zp) (xp', yp', zp') =
  ( x + (velocity_change xp xp')
  , y + (velocity_change yp yp')
  , z + (velocity_change zp zp'))

velocity_change n n'
  | n > n' = (-1)
  | n < n' = 1
  | otherwise = 0
