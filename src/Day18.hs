module Day18 where

import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Debug.Trace

type Coordinate = (Int, Int)

type Vault = Map.HashMap Coordinate Int

data State = State { curLoc :: Coordinate
                   , vault :: Vault
                   } deriving Show

data Search = Search { prevs :: HashSet.HashSet Coordinate
                     , loc :: Coordinate
                     , vs :: HashSet.HashSet Int
                     , br :: [(Int, Int)]}

main = do
  part1 "########################;#f.D.E.e.C.b.A.@.a.B.c.#;######################.#;#d.....................#;########################"

part1 :: String -> Int
part1 input =
  let state = parse_input input
      (_, searches) =
        List.foldl
        (\(st, acc) c ->
            let s =
                  Search { loc = curLoc state
                         , vs = HashSet.fromList []
                         , br = []
                         , prevs = HashSet.fromList [] }
            in (st, s:acc))
        (state, [])
        (next_coors (curLoc state) (HashSet.fromList []) (vault state))
  in bfs_search state 0 searches

bfs_search :: State -> Int -> [Search] -> Int
bfs_search state steps searches =
  let (state', searches') = List.foldl'
                            (\(st, acc) s ->
                                (process_each_path s state steps acc))
                            (state, [])
                            searches
  in case searches' of
       [] -> 0
       _ ->
         (bfs_search state' (steps + 1) searches')

process_each_path :: Search -> State -> Int -> [Search] -> (State, [Search])
process_each_path search state steps acc
  | length (vs search) == 16 =
      let steps' = trace (show steps) steps
      in case steps' of
           (-1) -> (state, acc)
           (-2) -> (state, acc)
  | val == Nothing =
      let state' = remove_coors state (br search)
      in (state', acc)
  | otherwise =
     let (Just n) = val
     in case n > 96 of
       True ->
         case HashSet.member n (vs search) of
           False ->
             let visited' = HashSet.insert n (vs search)
                 prev' = HashSet.insert (loc search) (prevs search)
             in trace
                ("steps: " ++ show steps ++ " visited: " ++ show visited')
                update_states
                state
                (search { vs = visited'
                        , prevs = HashSet.fromList []
                        , br = [] })
                acc
           True ->
             update_states state search acc
       False ->
         case n of
           0 ->
             update_states state search acc
           _ ->
             case HashSet.member (n + 32) (vs search) of
               True ->
                 update_states state search acc
               False -> (state, acc)
  where val = Map.lookup (loc search) (vault state)

update_states :: State -> Search -> [Search] -> (State, [Search])
update_states state search acc =
  let nextCoors = next_coors
                  (loc search)
                  (prevs search)
                  (vault state)
  in case nextCoors of
    [] ->
      let state' = remove_coors state (br search)
      in (state', acc)
    [co] ->
      let search' = search { loc = co
                           , prevs = HashSet.insert (loc search) (prevs search)
                           , br = co:(br search)}
      in (state, search':acc)
    _ ->
      (state,
       List.foldl'
       (\a c -> (search { loc = c
                        , br = []}) : a)
       acc
       (next_coors (loc search) (prevs search) (vault state)))

remove_coors :: State -> [(Int, Int)] -> State
remove_coors state branch =
  let v' =
        List.foldl'
        (\v c -> Map.delete c v)
        (vault state)
        branch
  in state {vault = v'}

next_coors :: (Int, Int) -> HashSet.HashSet Coordinate -> Vault -> [(Int, Int)]
next_coors (x, y) prev v =
  List.filter
  (\c -> not(HashSet.member c prev) && (Map.lookup c v /= Nothing))
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)]

parse_input :: String -> State
parse_input input =
  let lines = Split.splitOn ";" input
      initState = State { curLoc = (0, 0)
                        , vault = Map.fromList []}
  in parse_lines 0 lines initState

parse_lines :: Int -> [String] -> State -> State
parse_lines i [] state = state
parse_lines i (x:xs) state =
  let (state', _) =
        List.foldl' (\acc c -> update_vault acc i c) (state, 0) x
  in parse_lines (i + 1) xs state'

update_vault :: (State, Int) -> Int -> Char -> (State, Int)
update_vault (state, x) y c =
  let v = vault state
      loc = curLoc state
      v' = case c of
        '#' -> v
        '.' -> Map.insert (x, y) 0 v
        '@' -> Map.insert (x, y) 0 v
        _ -> Map.insert (x, y) (Char.ord c) v
      loc' = case c of
        '@' -> (x, y)
        _ -> loc
  in (state { curLoc = loc'
            , vault = v'}, x + 1)

