import System.Environment (getArgs)
import Text.Regex.Posix ((=~))
import Data.List (groupBy, tails, nubBy, sortBy)
import GHC.Data.FastString (FastString(uniq))
import Data.Function (on)
import Debug.Trace (traceShow)

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
  -- Find nodes that are also antinodes based on other nodes
  let triadsOfNodes = map triads groupedNodes
  let nodeAntinodes = map findNodeAntinodes triadsOfNodes
  let flattenedNodeAntinodes = concat nodeAntinodes
  let allAntinodes = flattenedAntinodes ++ flattenedNodeAntinodes
  let uniqueAntinodes = nubBy isSameCoords allAntinodes
  -- For remaining antinodes, filter out the nodes that are not on the map
  let antinodesOnMap = filter (`locationInMap` fileLines) uniqueAntinodes
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

findNodeAntinodes :: [(NodeLocation, NodeLocation, NodeLocation)] -> [Coords]
findNodeAntinodes [] = []
findNodeAntinodes ((node1, node2, node3):nodes) = do
  let (node1Type, node1Coords) = node1
  let (node2Type, node2Coords) = node2
  let (node3Type, node3Coords) = node3
  getTriadAntinode node1Coords node2Coords node3Coords ++ getTriadAntinode node2Coords node3Coords node1Coords ++ 
    getTriadAntinode node3Coords node2Coords node1Coords ++ findNodeAntinodes nodes

-- This is pretty shit, but I cbf refactoring it now I know how it's meant to look for these
getTriadAntinode :: Coords -> Coords -> Coords -> [Coords]
getTriadAntinode (x1, y1) (x2, y2) (x3, y3) = do
  [(x1, y1), (x2, y2), (x3, y3)]

findAntinodes :: [(NodeLocation, NodeLocation)] -> [Coords]
findAntinodes [] = []
findAntinodes ((node1, node2):nodes) = do
  let (node1Type, node1Coords) = node1
  let (node2Type, node2Coords) = node2
  getAntinodes node1Coords node2Coords ++ getAntinodes node2Coords node1Coords ++ findAntinodes nodes

getAntinodes :: Coords -> Coords -> [Coords]
getAntinodes (x1, y1) (x2, y2) = do 
  let xDiff = abs (x2 - x1)
  let yDiff = abs (y2 - y1)
  let x' = getOffsetCords x1 x2 xDiff 1
  let y' = getOffsetCords y1 y2 yDiff 1
  zip x' y'

-- Peeking at the input we know the max distance on the map is 50 so there can't be more than 48 antinodes
getOffsetCords :: Int -> Int -> Int -> Int -> [Int]
getOffsetCords a1 a2 diff 50 = []
getOffsetCords a1 a2 diff times = do
  if a1 > a2
  then a1 + diff * times : getOffsetCords a1 a2 diff (times + 1)
  else a1 - diff * times : getOffsetCords a1 a2 diff (times + 1)

pairs :: [NodeLocation] -> [(NodeLocation, NodeLocation)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triads :: [NodeLocation] -> [(NodeLocation, NodeLocation, NodeLocation)]
triads l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

isSameCoords :: Coords -> Coords -> Bool
isSameCoords (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

locationInMap :: Coords -> Graph -> Bool
locationInMap (x, y) graph = x >= 0 && y >= 0 && x < length (head graph) && y < length graph
