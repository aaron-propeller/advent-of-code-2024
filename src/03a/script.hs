import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let input = concat fileLines
          let mults = getAllTextMatches (input =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)") :: [String]
          let stringNums = map getNumbers mults
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
