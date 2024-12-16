import System.Environment (getArgs)
import Data.Maybe (isJust, isNothing)
import Control.Monad (guard)
import Data.List (nubBy)

type X = Int
type Y = Int
type Direction = Char
type CurrentLocation = (Maybe Direction, Maybe (X, Y))
type VisitedLocations = [CurrentLocation]
type Graph = [[Char]]

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let grid = fileLines
  let currentLocation = findCurrentLocation grid
  let visitedLocations = runSimulation grid currentLocation
  let uniqLocations = nubBy isSameCoord visitedLocations
  print $ length uniqLocations
  print "end"


findCurrentLocation :: Graph -> CurrentLocation
findCurrentLocation grid = iterateRows grid 0

iterateRows :: [[Char]] -> Y -> CurrentLocation
iterateRows [] _ = (Nothing, Nothing)
iterateRows (a:aa) y = do 
  let currentLocation = iterateColumns a 0 y
  if isJust (fst currentLocation)
  then currentLocation
  else iterateRows aa (y + 1)

iterateColumns :: [Char] -> X -> Y -> CurrentLocation
iterateColumns [] _ _ = (Nothing, Nothing)
iterateColumns (a:aa) x y = do 
  if isGuard a
  then (Just $ getDirection a, Just (x, y))
  else iterateColumns aa (x + 1) y

isGuard :: Char -> Bool
isGuard x = x == '>' || x == '<' || x == '^' || x == 'v'

getDirection :: Char -> Direction
getDirection x = do 
  if x == '^'
  then 'N'
  else do 
    if x == '>'
    then 'E'
    else do 
      if x == '<'
      then 'W'
      else 'S'

runSimulation :: Graph -> CurrentLocation -> VisitedLocations
runSimulation grid (_, Nothing) = []
runSimulation grid (Nothing, _) = []
runSimulation grid currentLocation = do
  let inFront = getInFront grid currentLocation
  let (Just direction, Just (x, y)) = currentLocation
  if inFront == Just '#'
  then runSimulation grid (Just $ turn direction, Just (x, y))
  else do 
    if isNothing inFront
    then [(Just direction, Just (x,y))]
    else do 
      let newLocation = (Just direction, Just (nextStep currentLocation))
      currentLocation : runSimulation grid newLocation

getInFront :: Graph -> CurrentLocation -> Maybe Char
getInFront grid currentLocation = do getElement grid (nextStep currentLocation)

nextStep :: CurrentLocation -> (X, Y)
nextStep (Nothing, _) = error "No direction"
nextStep (_, Nothing) = error "No location"
nextStep (Just direction, Just (x, y)) = do 
  if direction == 'N'
  then (x, y - 1)
  else do 
    if direction == 'E'
    then (x + 1, y)
    else do 
      if direction == 'W'
      then (x - 1, y)
      else (x, y + 1)

turn :: Direction -> Direction
turn direction = do 
  if direction == 'N'
  then 'E'
  else do 
    if direction == 'E'
    then 'S'
    else do 
      if direction == 'S'
      then 'W'
      else 'N'


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

isSameCoord :: CurrentLocation -> CurrentLocation -> Bool
isSameCoord (_, Just (x1, y1)) (_, Just (x2, y2)) = x1 == x2 && y1 == y2
