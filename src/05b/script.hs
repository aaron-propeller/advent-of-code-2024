import Text.Regex.Posix ((=~))
import Control.Monad (join)
import Data.List (sortBy)

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          -- Get all the rules and updates
          let rules = filter isRule fileLines
          let updates = filter isUpdate fileLines
          -- Create a regex for each rule
          let ruleRegexes = createRegexes rules
          -- Create a master rule that combines all the rules
          let masterRule = deleteLast $ join $ map (++"|") ruleRegexes
          -- Find all the invalid updates
          let invalidUpdates = findInvalidUpdates updates masterRule
          -- Convert the invalid updates to numbers
          let numberInvalidUpdates = convertToNumbers invalidUpdates
          -- Sort the invalid updates based on the rules
          let sortedUpdates = sortUpdates rules numberInvalidUpdates
          -- Get solution
          print $ sum $ map getMiddle sortedUpdates
          print "end"

isRule :: String -> Bool
isRule s = s =~ "^[0-9]+\\|[0-9]+$" :: Bool

isUpdate :: String -> Bool
isUpdate s = s =~ "^([0-9]+\\,)*[0-9]+$" :: Bool

createRegexes :: [String] -> [String]
createRegexes [] = []
createRegexes (rule:rules) = do 
  let [b, a] = wordsWhen (== '|') rule
  ("^([0-9]+\\,)*"++a++"\\,([0-9]+\\,)*"++b++"(\\,([0-9]+\\,)*([0-9]+$)|$)") : createRegexes rules

findValidUpdates :: [String] -> String -> [String]
findValidUpdates [] _ = []
findValidUpdates (update:updates) rule = do
  -- If our update matches the rule, ditch it
  if update =~ rule then findValidUpdates updates rule
  else update : findValidUpdates updates rule

findInvalidUpdates :: [String] -> String -> [String]
findInvalidUpdates [] _ = []
findInvalidUpdates (update:updates) rule = do
  if update =~ rule then update : findInvalidUpdates updates rule
  else findInvalidUpdates updates rule

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

deleteLast :: [Char] -> [Char]
deleteLast [] = error "Empty list!"
deleteLast [x] = []
deleteLast (x:xs) = x : deleteLast xs

getMiddle :: [Int] -> Int
getMiddle s = s !! div (length s) 2

convertToNumbers :: [[Char]] -> [[Int]]
convertToNumbers [] = []
convertToNumbers (s:ss) = do 
  let nums = wordsWhen (== ',') s
  map (\x -> read x :: Int) nums : convertToNumbers ss

sortUpdates :: [String] -> [[Int]] -> [[Int]]
sortUpdates rules [] = []
sortUpdates rules [x] = [sortBy (comparator rules) x]
sortUpdates rules (x:xs) = do
  let sorted = sortBy (comparator rules) x
  sorted : sortUpdates rules xs

comparator :: [String] -> Int -> Int -> Ordering
comparator rules a b = do
  if show a++"|"++show b `elem` rules then GT
  else LT
