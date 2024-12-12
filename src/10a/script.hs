import System.Environment (getArgs)
import Data.Map (fromList, Map, lookup)
import Debug.Trace (traceShow)
import Data.List (nub)
import Data.Bifunctor (Bifunctor(second))

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let topographicMap = lines file
  let indexes = findIndexes topographicMap '0'
  let trailSteps = fromList [('0', '1'), ('1', '2'), ('2', '3'), ('3', '4'), ('4', '5'), ('5', '6'), ('6', '7'), ('7', '8'), ('8', '9')]
  let trailheadScores = findScores topographicMap indexes trailSteps
  let uniqueTrailheadScores = map (second nub) trailheadScores
  let trailheadCounts = map (second length) uniqueTrailheadScores
  print (sum $ map snd trailheadCounts)
  print "end"


findIndexes :: Eq a => [[a]] -> a -> [(Int, Int)]
findIndexes matrix target = 
    [ (x, y) 
    | (x, row) <- zip [0..] matrix
    , (y, val) <- zip [0..] row
    , val == target
    ]


findScores :: [[Char]] -> [(Int, Int)] -> Map Char Char -> [((Int, Int), [(Int, Int)])]
findScores matrix indexes trailSteps = 
    [ ((x, y), score)
    | (x, y) <- indexes
    , let score = findScore matrix (x, y) trailSteps
    ]


findScore :: [[Char]] -> (Int, Int) -> Map Char Char -> [(Int, Int)]
findScore matrix (x, y) trailSteps = do
  let current = matrix !! x !! y
  if current == '9' then [(x, y)]
  else do
    let next = Data.Map.lookup current trailSteps
    case next of
      Nothing -> error "Invalid trail step"
      Just next -> do
        let neighbors = getNeighbors matrix (x, y)
        let validNeighbors = [ c | (n, c) <- neighbors, n == next ]
        if null validNeighbors then []
        else do
          let results = [ findScore matrix (x, y) trailSteps | (x, y) <- validNeighbors ]
          concat results

directions :: [(Int, Int)]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)] -- Up, Down, Left, Right

safeIndex :: [[a]] -> (Int, Int) -> Maybe a
safeIndex matrix (x, y) =
    if x >= 0 && x < length matrix && y >= 0 && y < length (head matrix)
        then Just ((matrix !! x) !! y)
        else Nothing

getNeighbors :: [[Char]] -> (Int, Int) -> [(Char, (Int, Int))]
getNeighbors matrix (x, y) =
    [ (value, neighbor)
    | (dx, dy) <- directions
    , let neighbor = (x + dx, y + dy)
    , Just value <- [safeIndex matrix neighbor]
    ]
