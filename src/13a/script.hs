import System.Environment (getArgs)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))
import Data.Maybe (fromMaybe)

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

testMachine :: Machine -> Int
testMachine ((ax, ay), (bx, by), (tx, ty)) = do 
  let possibilities = [ 
        (aPresses, bPresses) 
          | aPresses <- [0..100]
          , bPresses <- [0..100]
          , aPresses*ax + bPresses*bx == tx && aPresses*ay + bPresses*by == ty
          ]
  let costs = map (\(a, b) -> a*costA + b*costB) possibilities
  minWithDefault 0 costs

-- Safe minimum with default
minWithDefault :: (Ord a) => a -> [a] -> a
minWithDefault def xs = fromMaybe def (safeMinimum xs)

-- Helper: Safe minimum using Maybe
safeMinimum :: (Ord a) => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)
