import System.Environment (getArgs)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

type Velocity = (Int, Int)
type Position = (Int, Int)
type Robot = (Position, Velocity)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let details = map allMatches fileLines
  let robots = map (\[x, y, vx, vy] -> ((x, y), (vx, vy))) details
  let repositionedRobots = reposition robots iterations
  print $ map fst repositionedRobots
  -- split into 4 array based on the middles
  let (topLeft, topRight, bottomLeft, bottomRight) = foldl (\(tl, tr, bl, br) ((x, y), _) -> 
        if x < middleX && y < middleY
          then ((x, y):tl, tr, bl, br)
          else if x > middleX && y < middleY
            then (tl, (x, y):tr, bl, br)
            else if x < middleX && y > middleY
              then (tl, tr, (x, y):bl, br)
              else if x > middleX && y > middleY
                then (tl, tr, bl, (x, y):br)
                else (tl, tr, bl, br)
        ) ([], [], [], []) repositionedRobots 
  print $ length topLeft * length topRight * length bottomLeft * length bottomRight
  print "end"

allMatches :: String -> [Int]
allMatches str = map read $ getAllTextMatches (str =~ "-?[0-9]+")

iterations = 100

floorX = 101 
floorY = 103

middleX = floorX `div` 2 
middleY = floorY `div` 2

reposition :: [Robot] -> Int -> [Robot]
reposition robots 0 = robots
reposition robots n = do 
  let updatedRobots = map (\(pos, vel) -> (updatePosition pos vel, vel)) robots
  reposition updatedRobots (n - 1)

updatePosition :: Position -> Velocity -> Position 
updatePosition (x, y) (vx, vy) = do 
  let newX = updateCoord x vx floorX
  let newY = updateCoord y vy floorY
  (newX, newY)

-- lmao there has to be a better way to do this, but I'm too tired
updateCoord :: Int -> Int -> Int -> Int
updateCoord a ax max = do 
  let newA = a + ax 
  if newA >= max 
    then newA - max
    else if newA < 0
      then max + newA
      else newA
