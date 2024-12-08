import System.Environment (getArgs)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  print "end"
