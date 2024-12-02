
main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let stringReports = map words fileLines
          let reports = map (map read) stringReports :: [[Int]]
          print $ length reports
          let safeReports = filter reportIsSafe reports
          print $ length safeReports
          print "end"

reportIsSafe :: [Int] -> Bool
reportIsSafe [] = True
reportIsSafe [a] = True
reportIsSafe [a, b] = True
reportIsSafe [a, b, c] = allIncreasingSafe a b c || allDecreasingSafe a b c
reportIsSafe (a:b:c:xs) = (allIncreasingSafe a b c || allDecreasingSafe a b c) && reportIsSafe (b:c:xs)

allIncreasingSafe :: Int -> Int -> Int -> Bool
allIncreasingSafe a b c = a < b && b < c && b - a <= 3 && b - a >= 1 && c - b <= 3 && c - b >= 1

allDecreasingSafe :: Int -> Int -> Int -> Bool 
allDecreasingSafe a b c = a > b && b > c && a - b <= 3 && a - b >= 1 && b - c <= 3 && b - c >= 1
