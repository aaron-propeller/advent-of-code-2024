import System.Environment (getArgs)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

type ButtonA = (Int, Int)
type ButtonB = (Int, Int)
type Target = (Int, Int)
type Machine = (ButtonA, ButtonB, Target)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let numbers = map allMatches fileLines
  let numbersWithoutEmpty = filter (not . null) numbers
  let machines = makeMachines numbersWithoutEmpty
  let costs = map testMachine machines
  print costs
  print $ sum costs
  print "end"

allMatches :: String  -> [Int]
allMatches str = map read $ getAllTextMatches (str =~ "[0-9]+")

makeMachines :: [[Int]] -> [Machine]
makeMachines [] = []
makeMachines [[a, b]] = error "Invalid number of lines"
makeMachines [[a, b], [c, d]] = error "Invalid number of lines"
makeMachines [[a, b], [c, d], [e, f]] = [((a, b), (c, d), (e, f))]
makeMachines ([a, b]:[c, d]:[e, f]:xs) = ((a, b), (c, d), (e, f)) : makeMachines xs

costA = 3 
costB = 1
tOffset = 10000000000000

testMachine :: Machine -> Int
testMachine ((ax, ay), (bx, by), (tx, ty)) = do 
  let tx' = tx + tOffset 
  let ty' = ty + tOffset
  let bPresses = (ax*tOffset - ay*tOffset + ax*ty - ay*tx) `div` (ax*by - ay*bx)
  let aPresses = (ty + tOffset - bPresses*by) `div` ay
  -- Can't be bothered figuring out the floating point conversion, just recheck the equations
  if aPresses*ax + bPresses*bx == tx' && aPresses*ay + bPresses*by == ty'
    then aPresses*costA + bPresses*costB
    else 0
