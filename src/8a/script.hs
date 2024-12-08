import System.Environment (getArgs)
import Text.Regex.Posix ((=~))
import Data.List (groupBy, tails, nubBy, sortBy)
import GHC.Data.FastString (FastString(uniq))
import Data.Function (on)

type Graph = [[Char]]
type X = Int
type Y = Int
type Coords = (X, Y)
type NodeType = Char
type NodeLocation = (NodeType, Coords)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  -- For every pair of nodes there will be two antinodes 
  -- Find all nodes of each type
  let nodes = iterateRows fileLines 0
  -- For each list of nodes, find all pairs of nodes 
  let sortedNodes = sortBy (compare `on` fst) nodes
  let groupedNodes = groupBy ((==) `on` fst) sortedNodes
  let pairsOfNodes = map pairs groupedNodes
  -- For each pair of nodes, find the antinodes
  let antinodes = map findAntinodes pairsOfNodes
  let flattenedAntinodes = concat antinodes
  -- Filter out unique anitnodes
  let uniqAntinodes = nubBy isSameCoords flattenedAntinodes
  -- For remaining antinodes, filter out the nodes that are not on the map
  let antinodesOnMap = filter (`locationInMap` fileLines) uniqAntinodes
  print $ length antinodesOnMap
  print "end"

iterateRows :: [String] -> Int -> [NodeLocation]
iterateRows [] _ = []
iterateRows (row:rows) y = iterateCols row 0 y ++ iterateRows rows (y + 1)

iterateCols :: String -> Int -> Int -> [NodeLocation]
iterateCols [] _ _ = []
iterateCols (cell:cells) x y = do 
  if isNode cell
  then (cell, (x, y)) : iterateCols cells (x + 1) y
  else iterateCols cells (x + 1) y

isNode :: Char -> Bool 
isNode x = [x] =~ "[A-Za-z0-9]"

findAntinodes :: [(NodeLocation, NodeLocation)] -> [Coords]
findAntinodes [] = []
findAntinodes ((node1, node2):nodes) = do
  let (node1Type, node1Coords) = node1
  let (node2Type, node2Coords) = node2
  getAntinode node1Coords node2Coords : getAntinode node2Coords node1Coords : findAntinodes nodes

getAntinode :: Coords -> Coords -> Coords
getAntinode (x1, y1) (x2, y2) = do 
  let xDiff = abs (x2 - x1)
  let yDiff = abs (y2 - y1)
  let x' = getOffsetCord x1 x2 xDiff
  let y' = getOffsetCord y1 y2 yDiff
  (x', y')

getOffsetCord :: Int -> Int -> Int -> Int
getOffsetCord a1 a2 diff = do
  if a1 > a2
  then a1 + diff
  else a1 - diff

pairs :: [NodeLocation] -> [(NodeLocation, NodeLocation)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

isSameCoords :: Coords -> Coords -> Bool
isSameCoords (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

locationInMap :: Coords -> Graph -> Bool
locationInMap (x, y) graph = x >= 0 && y >= 0 && x < length (head graph) && y < length graph
