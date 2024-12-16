import System.Environment (getArgs)
import Data.Maybe (isNothing, isJust, fromJust, catMaybes)
import Debug.Trace (traceShow, trace)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let input = head fileLines
  let blockInfo = map (\x -> read [x] :: Int) input
  let blockRepresentation = formatBlocks blockInfo 0 0
  let slim = defrag blockRepresentation
  let slimFlat = concat slim
  let sum = multiIndexSum slimFlat 0
  print sum
  print "end"

formatBlocks :: [Int] -> Int -> Int -> [[Maybe Int]]
formatBlocks [] _ _ = []
formatBlocks blocks index id = do 
  let (x:xs) = blocks
  if even index
  then
    createArray (Just id) x : formatBlocks xs (index + 1) (id + 1)
  else
    createArray Nothing x : formatBlocks xs (index + 1) id

createArray :: Maybe Int -> Int -> [Maybe Int]
createArray _ 0 = []
createArray val x = val : createArray val (x - 1)

defrag :: [[Maybe Int]] -> [[Maybe Int]]
defrag [] = []
defrag blocks = do 
  let x = last blocks
  let remainingBlocks = init blocks
  if Nothing `elem` x
  then do
    defrag remainingBlocks ++ [x]
  else do
    let (found, filled) = fillSpace remainingBlocks x
    if found
    then do
      defrag filled ++ [createArray Nothing (length x)]
    else do
      defrag filled ++ [x]

fillSpace :: [[Maybe Int]] -> [Maybe Int] -> (Bool, [[Maybe Int]])
fillSpace [] x = (False, [])
fillSpace remainingBlocks toInsert = do 
  let toInsertSize = length toInsert 
  let (x:xs) = remainingBlocks 
  if Nothing `elem` x && length x >= toInsertSize
  then do 
    let remainingSpace = length x - toInsertSize
    let result = toInsert : createArray Nothing remainingSpace : xs
    (True, result)
  else do 
    let (found, remainingBlocks) = fillSpace xs toInsert
    (found, x : remainingBlocks)

multiIndexSum :: [Maybe Int] -> Int -> Int
multiIndexSum [] _ = 0 
multiIndexSum [Nothing] _ = 0
multiIndexSum [Just x] index = x*index
multiIndexSum (Nothing:xs) index = multiIndexSum xs (index + 1)
multiIndexSum ((Just x):xs) index = x*index + multiIndexSum xs (index + 1)
