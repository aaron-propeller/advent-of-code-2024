import Data.List (sort)
import Data.Map (Map, fromListWith, findWithDefault, size)

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let entries = map parseInput fileLines
          -- Reduce entries to arrays of left and right
          let (left, right) = reduceEntries entries
          -- Sort lists
          let sortedLeft = sort left
          let rightMap = countElements right
          let val = iterateLists sortedLeft rightMap
          print val

parseInput :: String -> (Int, Int)
parseInput input = (read $ head $ words input, read $ last $ words input)

countElements :: [Int] -> Map Int Int
countElements = fromListWith (+) . flip zip (repeat 1)

reduceEntries :: [(Int, Int)] -> ([Int], [Int])
reduceEntries = foldl (\(left, right) (l, r) -> (l:left, r:right)) ([], [])

iterateLists :: [Int] -> Map Int Int -> Int
iterateLists [] map = 0
iterateLists ls map = (head ls * findWithDefault 0 (head ls) map) + iterateLists (tail ls) map
