import Control.Monad (guard)
type X = Int
type Y = Int
type CountValidSearch = Int
type Graph = [[Char]]
type CurrentLocation = ([[Char]], X, Y)

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let count = iterateRows fileLines fileLines (0,0)
          print count
          print "end"

iterateRows :: Graph -> Graph -> (X, Y) -> CountValidSearch
iterateRows graph [] (x,y) = 0 
iterateRows graph (row:rows) (x,y) = iterateColumns graph row (x,y) + iterateRows graph rows (x, y + 1)

iterateColumns :: Graph -> [Char] -> (X, Y) -> CountValidSearch 
iterateColumns graph [] (x,y) = 0 
iterateColumns graph (cell:cells) (x,y) = do 
  let currentLocation = (graph, x, y)
  if cell == 'A' then
    checkCorners currentLocation + iterateColumns graph cells (x + 1, y)
  else
    iterateColumns graph cells (x + 1, y)

checkCorners :: CurrentLocation -> CountValidSearch
checkCorners (graph, x, y) = do 
  let northEast = getElement graph (x+1, y-1)
  let northWest = getElement graph (x-1, y-1)
  let southEast = getElement graph (x+1, y+1)
  let southWest = getElement graph (x-1, y+1) 
  let corners = [northEast, northWest, southEast, southWest]
  let mCount = length $ filter (== Just 'M') corners 
  let sCount = length $ filter (== Just 'S') corners
  if mCount == 2 && sCount == 2  && northEast /= southWest then
    1
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
