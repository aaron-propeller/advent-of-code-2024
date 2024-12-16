import Data.List (sort)

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let entries = map parseInput fileLines
          -- Reduce entries to arrays of left and right
          let (left, right) = reduceEntries entries
          -- Sort lists
          let sortedLeft = sort left
          let sortedRight = sort right
          let val = iterateLists sortedLeft sortedRight
          print val

parseInput :: String -> (Int, Int)
parseInput input = (read $ head $ words input, read $ last $ words input)


reduceEntries :: [(Int, Int)] -> ([Int], [Int])
reduceEntries = foldl (\(left, right) (l, r) -> (l:left, r:right)) ([], [])

iterateLists :: [Int] -> [Int] -> Int
iterateLists [] [] = 0
iterateLists [] rs = 0 
iterateLists ls [] = 0 
iterateLists ls rs = abs (head ls - head rs) + iterateLists (tail ls) (tail rs)


