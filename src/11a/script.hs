import System.Environment (getArgs)
import Data.Map (Map, insertWith, empty, toList)
import GHC.Plugins (stateHackOneShot)
import Data.Bifunctor (Bifunctor(first))
import Debug.Trace (traceShow)

type Stones = Map Int Int

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  -- split first line on spaces and convert to Int
  let input = map read $ words $ head fileLines :: [Int]
  -- Turn into Map of count of each stone
  let stones = foldl (\acc x -> insertWith (+) x 1 acc) empty input
  let stonesAfterBlinking = blink 25 stones
  let stoneCount = foldl (\acc (_, v) -> acc + v) 0 $ toList stonesAfterBlinking
  print stoneCount
  print "end"

blink :: Int -> Stones -> Stones
blink 0 stones = stones 
blink n stones = do 
  let newStones = splitStones $ toList stones
  blink (n-1) newStones

splitStones :: [(Int, Int)] -> Stones 
splitStones stones = do
  let newStones = map (\(stone, numberOfThatStone) -> (handleSplit stone, numberOfThatStone)) stones
  foldl (\acc (stonesToAdd, amount) -> foldl (\acc x -> insertWith (+) x amount acc) acc stonesToAdd) empty newStones

handleSplit :: Int -> [Int]
handleSplit num
  | num == 0 = [1]
  | even $ length $ show num = do
      let numStr = show num 
      let half = length numStr `div` 2
      let firstHalf = read $ take half numStr :: Int 
      let secondHalf = read $ drop half numStr :: Int 
      [firstHalf, secondHalf]
  | otherwise = [num*2024]
