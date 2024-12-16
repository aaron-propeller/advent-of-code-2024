import System.Environment (getArgs)
import Data.Map (Map, insertWith, empty, toList, lookupMin, (!?), (!), singleton, (\\), keys, findWithDefault)
import Data.Maybe (isJust)
import Data.Bifunctor (Bifunctor(bimap))

type Cell = (Int, Int)
type Neighbor = Cell 
type Neighbors = [Neighbor]
type Area = Int
type Corners = Int
type PlotType = Char
type Graph = Map Cell (PlotType, Neighbors)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let garden = lines file
  let graph = toGraph garden  
  let plots = findPlots graph graph
  let costs = foldl (\acc (plotType, area, corners) -> acc + area * corners) 0 plots
  print costs
  print "end"

toGraph :: [[Char]] -> Graph
toGraph garden =
  let cells = [ (x, y, plotType) | (x, row) <- zip [0..] garden, (y, plotType) <- zip [0..] row]
      graph = empty
      graph' = foldl 
        (\acc (x, y, plotType) -> 
            insertWith 
                (\_ (p, n) -> (p, n)) 
                (x, y) 
                (plotType, findNeighbors (x, y)) 
                acc
        ) 
        graph 
        cells
  in graph'

directions :: [Cell]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)] -- Up, Down, Left, Right

corners :: [(Cell, Cell, Cell)]
corners = [((0, -1), (-1, -1), (-1, 0)), ((0, -1), (1, -1), (1, 0)), ((-1, 0), (-1, 1), (0, 1)), ((0, 1), (1, 1), (1, 0))] -- Top left, Top right, Bottom left, Bottom right

findNeighbors :: (Int, Int) -> Neighbors
findNeighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

-- Starting with the first element, iterate all neighbours, taking any that have the same plot type, returning a Graph with those values
-- Turn that graph into tuple (PlotType, Area, Perimeter)
-- Drop all elements from the original graph that are in the new graph 
-- Repeat until the original graph is empty
findPlots :: Graph -> Graph -> [(PlotType, Int, Int)]
findPlots originalGraph graph = do 
  let plot = lookupMin graph
  case plot of
    Just (cell, (plotType, _)) -> do
      let foundPlot = findPlot originalGraph [cell] cell
      let (area, perimeter) = foldl (\(a, p) (_, (a', p')) -> (a + a', p + p')) (0, 0) (toList foundPlot)
      (plotType, area, perimeter) : findPlots originalGraph (graph \\ foundPlot)
    Nothing -> []

-- Get current cell details 
-- Get all neighbours from the graph 
-- Filter for nieghbours that have the same plot type
-- Calculate the area and perimeter for the current cell 
-- For each neighbour recursively find the area and perimeter 
-- Combine graphs from neighbours and current cell into 1 
-- return that graph
-- TODO Part B: Instead of neighbours get the three corner cells. If all 3 are different, then it's a corner cell. If there is 1 different on the diagonal and the other 2 are of the same type, then it is a side cell.
findPlot :: Graph -> [Cell] -> Cell -> Map Cell (Area, Corners)
findPlot originalGraph visited cell = do
  let (plotType, neighbors) = originalGraph ! cell 
  -- Account for neighbors that are off the grid
  let neighbors' = filter (\n -> isJust (originalGraph !? n) && fst (originalGraph ! n) == plotType) neighbors
  let unvisitedNeighbors = filter (`notElem` visited) neighbors'
  let area = 1
  -- We can get away with this because we add neighbors that are off the grid
  let corners = findCorners originalGraph cell
  let currentGraph = singleton cell (area, corners)
  let visited' = visited ++ unvisitedNeighbors
  -- fold over unvisited neighbors, using the current graph as the accumulator and using the keys of the accumulator as the visited list 
  let combinedGraphs = foldl (\acc (x, y) -> (<>)(findPlot originalGraph (keys acc ++ visited') (x, y)) acc) currentGraph unvisitedNeighbors
  combinedGraphs

findCorners :: Graph -> Cell -> Corners
findCorners graph cell = do
  let validCorners = filter (\(c1, c2, c3) -> isCorner graph cell c1 c2 c3) corners
  length validCorners

isCorner :: Graph -> Cell -> Cell -> Cell -> Cell -> Bool
isCorner graph cell offset1 offset2 offset3 = do
  let c1 = bimap (fst cell +) (snd cell +) offset1
  let c2 = bimap (fst cell +) (snd cell +) offset2 
  let c3 = bimap (fst cell +) (snd cell +) offset3
  let (currentPlotType, _) = findWithDefault (' ', []) cell graph
  let (plotType', _) = findWithDefault (' ', []) c1 graph
  let (diagonalPlotType'', _) = findWithDefault (' ', []) c2 graph
  let (plotType''', _) = findWithDefault (' ', []) c3 graph
  isInsideCorner currentPlotType plotType' plotType''' || isOutsideCorner currentPlotType plotType' diagonalPlotType'' plotType''' 

isInsideCorner :: PlotType -> PlotType -> PlotType -> Bool 
isInsideCorner currentPlotType plotType' plotType''' = do
  currentPlotType /= plotType' && currentPlotType /= plotType'''

isOutsideCorner :: PlotType -> PlotType -> PlotType -> PlotType -> Bool 
isOutsideCorner currentPlotType plotType' diagonalPlotType'' plotType''' = do
  currentPlotType == plotType' && currentPlotType /= diagonalPlotType'' && currentPlotType == plotType'''
