module Scoring where

import System.Directory (listDirectory)
import Text.Read (readMaybe)
import Data.Strings (strSplitAll)

data Gender = Male | Female deriving (Show, Eq )

stringToGender :: String -> Maybe Gender
stringToGender ('m':_) = Just Male
stringToGender ('M':_) = Just Male
stringToGender ('f':_) = Just Female
stringToGender ('F':_) = Just Female
stringToGender _ = Just Female

data Athlete = Athlete { athleteName :: String , age :: Maybe Int , sex :: Gender }

athFromString :: String -> [String]
athFromString line = strSplitAll "," line

data RaceData = RaceData { raceName :: String , raceDate :: String , points :: Int , athLines :: [[String]]} deriving (Show)

path :: String
path = "data"

processHeader:: [String] -> Maybe RaceData
processHeader (name:date:_:p2:rest) =
  case (readMaybe p2) of
    Just(x) -> Just $ RaceData { raceName=name, raceDate = date , points = x , athLines = map athFromString rest }
    _ -> Nothing
processHeader _ = Nothing

readAFile:: String -> IO (Maybe RaceData)
readAFile fn = do
  let fullpath = (path ++ "/" ++ fn)
  input <- readFile fullpath
  return (processHeader (lines input))

scoring :: IO ()
scoring = do
  files <- listDirectory path
  res <- traverse readAFile files
  print res

