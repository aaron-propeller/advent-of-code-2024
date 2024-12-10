import System.Environment (getArgs)
import Data.Maybe (isNothing, isJust, fromJust, catMaybes)
import System.Posix (BaudRate(B0))

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let input = head fileLines
  let blockInfo = map (\x -> read [x] :: Int) input
  let blockRepresentation = formatBlocks blockInfo 0 0
  let slim = defrag blockRepresentation
  let realSlim = catMaybes slim
  let sum = multIndexSum realSlim 0
  print sum
  print "end"

formatBlocks :: [Int] -> Int -> Int -> [Maybe Int]
formatBlocks [] _ _ = []
formatBlocks blocks index id = do 
  let (x:xs) = blocks
  if even index
  then
    createArray (Just id) x ++ formatBlocks xs (index + 1) (id + 1)
  else
    createArray Nothing x ++ formatBlocks xs (index + 1) id

createArray :: Maybe Int -> Int -> [Maybe Int]
createArray _ 0 = []
createArray val x = val : createArray val (x - 1)

defrag :: [Maybe Int] -> [Maybe Int]
defrag [] = []
defrag (x:xs) = do 
  case x of
    Nothing -> do 
      let (y, xs') = takeLast xs
      case y of
        Nothing -> defrag xs'
        Just y -> Just y : defrag xs'
    Just y -> Just y : defrag xs

takeLast :: [Maybe Int] -> (Maybe Int, [Maybe Int])
takeLast [] = (Nothing, [])
takeLast xs = do 
  let (y:ys) = reverse xs
  if isNothing y
  then
    takeLast $ reverse ys
  else
    (y, reverse ys)

multIndexSum :: [Int] -> Int -> Int
multIndexSum [] _ = 0 
multIndexSum [x] index = x*index 
multIndexSum (x:xs) index = x*index + multIndexSum xs (index + 1)

