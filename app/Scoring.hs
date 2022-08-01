module Scoring where

import System.Directory (listDirectory)
import Text.Read (readMaybe)
import Data.Strings (strSplitAll)
import Data.Maybe (mapMaybe)
import Data.Time

import Data.Int
import Data.Ratio

--produce an infinite list of score factors
scoreFor :: Integer -> Rational
scoreFor x = 5 % (4+x) :: Rational
scores = map scoreFor [1..]

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
    (_:name:age:sex:_) -> Just $ Athlete { athleteName = name, age = (readMaybe age::Maybe Int), sex = stringToGender sex }
    _ -> Nothing

data RaceData = RaceData { raceName :: String , raceDate :: Day , points :: Int , athLines :: [Athlete]} deriving (Show)

path :: String
path = "data"

parseDate :: String -> Day
parseDate dateString = case (mapMaybe (\s -> (readMaybe s)::Maybe Int) (strSplitAll "-" dateString)) of
  [y,m,d] -> fromGregorian (toInteger y) m d

processHeader:: [String] -> Maybe RaceData
processHeader (name:date:_:p2:rest) =
  case (readMaybe p2) of
    Just(x) -> Just $ RaceData { raceName=name, raceDate = parseDate date , points = x , athLines = mapMaybe athFromString rest }
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

