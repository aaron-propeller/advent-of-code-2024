import System.Environment (getArgs)
import Data.Int (Int64)

type Number = Int64
type Target = Number
type Operation = Number -> Number -> Number
type Operations = [Operation]
type OperationsLeftForCurrentNumber = [Operation]
type NumbersToValidate = [Number]
type CurrentValue = Number


main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let strippedLines = map (filter (\x -> x `elem` ['0'..'9'] ++ [' '])) fileLines
  let calibrations = map (map read . words) strippedLines :: [[Number]]
  let validCalibrations = filter validateCalibration calibrations
  let targets = map head validCalibrations
  print $ sum targets
  print "end"


validateCalibration :: [Number] -> Bool
validateCalibration (target:values) = do 
  let operations = [(+), (*), combineNumbers]
  runValidation target operations operations values 0


runValidation :: Target -> Operations -> OperationsLeftForCurrentNumber -> NumbersToValidate -> CurrentValue -> Bool
runValidation _ _ [] _ _ = False
runValidation _ _ _ [] _ = False
runValidation target operations operationsLeftForCurrentNumber numbersToValidate currentValue = do 
  let (currentNumber:remainingNumbers) = numbersToValidate
  let (currentOperation:remainingOperationsForCurrentNumber) = operationsLeftForCurrentNumber
  let newCurrentValue = currentOperation currentValue currentNumber
  -- Optimise later, if current value is already greater than target, skip the remaining numbers and just do operations
  (null remainingNumbers && newCurrentValue == target) ||
    runValidation target operations operations remainingNumbers newCurrentValue ||
    runValidation target operations remainingOperationsForCurrentNumber numbersToValidate currentValue


combineNumbers :: Number -> Number -> Number
combineNumbers x y = read $ show x ++ show y :: Number
