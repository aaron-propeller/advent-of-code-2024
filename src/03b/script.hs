import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

main :: IO ()
main = do file <- readFile "sample2.txt"
          let fileLines = lines file
          let input = concat fileLines
          let regex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"
          let mults = getAllTextMatches (input =~ regex) :: [String]
          let validMults = takeDos mults True
          let stringNums = map getNumbers validMults
          let nums = map (wordsWhen (==',')) stringNums
          let appliedMults = map product nums
          let total = sum appliedMults
          print total
          print "end"

getNumbers :: String -> String
getNumbers s = head $ getAllTextMatches (s =~ "[0-9]{1,3},[0-9]{1,3}")


wordsWhen     :: (Char -> Bool) -> String -> [Int]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> read w : wordsWhen p s''
                    where (w, s'') = break p s'

takeDos :: [String] -> Bool -> [String]
takeDos [] _ = []
takeDos ("do()":xs) _ = takeDos xs True
takeDos ("don't()":xs) _ = takeDos xs False
takeDos (x:xs) True = x : takeDos xs True
takeDos (x:xs) False = takeDos xs False
