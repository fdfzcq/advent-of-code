module Day11 where

import Computer
import qualified Data.Map as Map
import qualified Data.List as List

data Robot = Robot
  { panel :: Panel
  , direction :: Direction
  , state :: ComputerState
  , panelMap :: PanelMap
  } deriving Show
type Panel = (Int, Int)
-- facing
-- 0: up    -> (x, y - 1)
-- 1: right -> (x + 1, y)
-- 2: down  -> (x, y + 1)
-- 3: left  -> (x - 1, y)
type Direction = Int
type PanelMap = Map.Map Panel Color
type Color = Int -- 0 : black, 1 : white

defaultColor = 0

-- part 1
registration_identifier :: [Int] -> IO ()
registration_identifier xs =
  let initialCS = Computer.initial_computer_state xs []
      initDir = 0
      initPanel = (0, 0)
      initPanelMap = Map.fromList [((0,(-1)),1)]
      robot = Robot { panel = initPanel
                    , direction = initDir
                    , state = initialCS
                    , panelMap = initPanelMap }
--      robot' = paint_panels robot
--  in collect_result robot'
  in visualise_result (paint_panels robot)

visualise_result :: Robot -> IO ()
visualise_result robot =
  let panels = Map.toList (panelMap robot)
      whitePanels = List.foldl (\acc (x, c) ->
                                   case c of
                                     1 -> (x:acc)
                                     _ -> acc
                               ) [] panels
      (xMin, yMin, xMax, yMax) = find_border whitePanels
      spaceShip = List.map (\a -> List.map (\b -> (b, a)) [xMin..xMax]) [yMin..yMax]
  in putStrLn (plot spaceShip whitePanels)

find_border :: [Panel] -> (Int, Int, Int, Int)
find_border panels =
  let (xMin, _) = List.minimumBy (\(x1, y1) (x2, y2) -> compare x1 x2) panels
      (_, yMin) = List.minimumBy (\(x1, y1) (x2, y2) -> compare y1 y2) panels
      (xMax, _) = List.maximumBy (\(x1, y1) (x2, y2) -> compare x1 x2) panels
      (_, yMax) = List.maximumBy (\(x1, y1) (x2, y2) -> compare y1 y2) panels
  in (xMin, yMin, xMax, yMax)

plot :: [[Panel]] -> [Panel] -> String
plot spaceShip whitePanels =
  List.foldl (\acc line ->
                let str = List.foldl (\a p ->
                                        case List.elem p whitePanels of
                                          True -> a ++ "0"
                                          False -> a ++ "_"
                                     ) "" line
                in acc ++ "\n" ++ str
             ) "" spaceShip

collect_result :: Robot -> Int
collect_result robot =
  let pm = panelMap robot
  in length (Map.toList pm)

paint_panels :: Robot -> Robot
paint_panels robot =
  let robot' = paint_panel robot
  in case pointer (state robot') of
       (-1) -> robot'
       _ -> paint_panels robot'

paint_panel :: Robot -> Robot
paint_panel robot =
  let newPanel = move (panel robot) (direction robot)
      input = panel_lookup newPanel (panelMap robot)
      cs = state robot
      newCs = process_until_halt (cs {inputs = [input]})
      --
      [color, dirInstruction] = outputs newCs
      newDir = turn (direction robot) dirInstruction
      newPanelMap = Map.insert newPanel color (panelMap robot)
  in robot { panel = newPanel
           , direction = newDir
           , state = newCs {outputs = [], inputs = [color]}
           , panelMap = newPanelMap }

turn :: Direction -> Int -> Direction
turn dir 0
  | dir == 0 = 3
  | otherwise = dir - 1
turn dir 1
  | dir == 3 = 0
  | otherwise = dir + 1

move :: Panel -> Int -> Panel
move (x, y) 0 = (x, y - 1)
move (x, y) 1 = (x + 1, y)
move (x, y) 2 = (x, y + 1)
move (x, y) 3 = (x - 1, y)

panel_lookup :: Panel -> PanelMap -> Color
panel_lookup panel panelMap =
  case Map.lookup panel panelMap of
    (Just color) -> color
    Nothing -> defaultColor
