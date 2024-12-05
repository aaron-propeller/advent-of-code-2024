import Text.Regex.Posix ((=~))
import Control.Monad (join)

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let rules = filter isRule fileLines
          let updates = filter isUpdate fileLines
          let ruleRegexes = createRegexes rules
          let masterRule = deleteLast $ join $ map (++"|") ruleRegexes
          let validUpdates = findValidUpdates updates masterRule
          print $ sum $ map getMiddle validUpdates
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

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

deleteLast :: [Char] -> [Char]
deleteLast [] = error "Empty list!"
deleteLast [x] = []
deleteLast (x:xs) = x : deleteLast xs

getMiddle :: [Char] -> Int
getMiddle s = do
  let nums = wordsWhen (== ',') s 
  let middle = div (length nums) 2 
  read (nums !! middle) :: Int
