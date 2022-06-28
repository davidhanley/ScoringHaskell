import System.Directory (listDirectory)
import Text.Read (readMaybe)

path :: String
path = "data"

proc:: [String] -> Integer
proc (name:date:_:p2:rest) =
  case (readMaybe p2) of
    Nothing -> 0
    Just x -> x + 1
proc _ = 0

readAFile:: String -> IO Integer
readAFile fn = do
  let fullpath = (path ++ "/" ++ fn)
  input <- readFile fullpath
  return (proc (lines input))

main :: IO ()
main = do
  files <- listDirectory path
  res <- traverse readAFile files
  print res 

