
main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let stringReports = map words fileLines
          let reports = map (map read) stringReports :: [[Int]]
          let safeReports = filter reportIsSafe reports
          print $ length safeReports
          print "end"

reportIsSafe :: [Int] -> Bool
reportIsSafe xs = stableSafe xs || dampenedSafe xs

stableSafe :: [Int] -> Bool
stableSafe [] = True
stableSafe [a] = True
stableSafe [a, b] = True
stableSafe [a, b, c] = allIncreasingSafe a b c || allDecreasingSafe a b c
stableSafe (a:b:c:xs) = (allIncreasingSafe a b c || allDecreasingSafe a b c) && stableSafe (b:c:xs)

allIncreasingSafe :: Int -> Int -> Int -> Bool
allIncreasingSafe a b c = a < b && b < c && b - a <= 3 && b - a >= 1 && c - b <= 3 && c - b >= 1

allDecreasingSafe :: Int -> Int -> Int -> Bool 
allDecreasingSafe a b c = a > b && b > c && a - b <= 3 && a - b >= 1 && b - c <= 3 && b - c >= 1

dampenedSafe :: [Int] -> Bool 
dampenedSafe xs = do 
  let n = length xs
  removeAtAndCheckSafe xs n

removeAtAndCheckSafe :: [Int] -> Int -> Bool
removeAtAndCheckSafe xs 0 = do
  let xs' = remove 0 xs
  stableSafe xs'
removeAtAndCheckSafe xs n = do
  let xs' = remove n xs
  stableSafe xs' || removeAtAndCheckSafe xs (n - 1)

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs
