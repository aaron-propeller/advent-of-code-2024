import Control.Monad (guard)
type X = Int
type Y = Int
type Direction = (X, Y)
type ValidSearch = Bool
type CountValidSearch = Int
type Graph = [[Char]]
type CurrentLocation = (Graph, X, Y)
type Pattern = [Char]

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let count = iterateRows (fileLines,0,0) fileLines

          print count
          print "end"

iterateRows :: CurrentLocation -> [[Char]] -> CountValidSearch
iterateRows currentLocation [] = 0 
iterateRows (graph,x,y) (row:rows) = iterateColumns (graph,x,y) row + iterateRows (graph,x,y+1) rows

iterateColumns :: CurrentLocation -> [Char] -> CountValidSearch 
iterateColumns currentLocation [] = 0 
iterateColumns (graph,x,y) (cell:cells) = do 
  let currentLocation = (graph, x, y)
  let toFind = ['M', 'A', 'S']
  let directions = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (-1, -1), (1, 1), (-1, 1)]
  if cell == 'X' then
    checkDirections currentLocation directions toFind + iterateColumns (graph,x+1,y) cells
  else
    iterateColumns (graph,x+1,y) cells


checkDirections :: CurrentLocation -> [Direction] -> Pattern -> CountValidSearch
checkDirctions currentLocation [] pattern = 0
checkDirections currentLocation [x] pattern = checkDirection currentLocation x pattern
checkDirections currentLocation (x:xs) pattern = checkDirection currentLocation x pattern + checkDirections currentLocation xs pattern

checkDirection :: CurrentLocation -> Direction -> Pattern -> CountValidSearch
checkDirection currentLocation direction [] = 1
checkDirection (graph, x, y) (xOffset, yOffset) (find:toFind) = do 
  let x' = x + xOffset
  let y' = y + yOffset
  let element = getElement graph (x', y')
  case element of
    Nothing -> 0
    Just element -> do 
      if element == find then
        checkDirection (graph, x', y') (xOffset, yOffset) toFind
      else 
        0

getElement :: Graph -> (X, Y) -> Maybe Char
getElement graph (x, y) = do 
  guard $ withinYBounds graph y
  guard $ withinXBounds graph x
  return $ (graph!!y)!!x

withinXBounds :: Graph -> X -> Bool
withinXBounds graph x = x >= 0 && x < graphWidth graph 

withinYBounds :: Graph -> Y -> Bool 
withinYBounds graph y = y >= 0 && y < graphHeight graph 

graphHeight :: Graph -> Int
graphHeight = length

graphWidth :: Graph -> Int
graphWidth graph = length $ head graph
