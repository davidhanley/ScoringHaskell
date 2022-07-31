module Scoring where

import System.Directory (listDirectory)
import Text.Read (readMaybe)
import Data.Strings (strSplitAll)
import Data.Maybe (mapMaybe)

data Gender = Male | Female deriving (Show, Eq )

stringToGender :: String -> Maybe Gender
stringToGender ('m':_) = Just Male
stringToGender ('M':_) = Just Male
stringToGender ('f':_) = Just Female
stringToGender ('F':_) = Just Female
stringToGender _ = Just Female

data Athlete = Athlete { athleteName :: String , age :: Maybe Int , sex :: Maybe Gender } deriving (Show, Eq )

athFromString :: String -> Maybe Athlete
athFromString line =
  case (strSplitAll "," line) of
    (_:name:age:sex:_) -> Just $ Athlete { athleteName = name ,age = (readMaybe age::Maybe Int)  , sex = stringToGender sex }
    _ -> Nothing



data RaceData = RaceData { raceName :: String , raceDate :: String , points :: Int , athLines :: [Athlete]} deriving (Show)

path :: String
path = "data"

processHeader:: [String] -> Maybe RaceData
processHeader (name:date:_:p2:rest) =
  case (readMaybe p2) of
    Just(x) -> Just $ RaceData { raceName=name, raceDate = date , points = x , athLines = mapMaybe athFromString rest }
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

