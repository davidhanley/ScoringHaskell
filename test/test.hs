import Test.HUnit

import Scoring
--import Data.Char

--test1 :: Test
--test1 = TestCase (assertEqual "test1" (stringToGender "Male") (Just Male) )

--test2 :: Test
--test2 = TestCase (assertEqual "test1" (stringToGender "m") (Just Male) )

test2 :: Test
test2 = TestList [ "First STG test"   ~: (Just Male) ~=? (stringToGender "Male")
                 , "Second STG test"  ~: (Just Male) ~=? (stringToGender "m")
                 , "Third STG test"  ~: (Just Female) ~=? (stringToGender "f")
                 , "Fourth STG test"  ~: (Just Female) ~=? (stringToGender "Female")
                 , "Fifth STG test"  ~: Nothing ~=? (stringToGender "Bobby")
                 , "Sixth STG test"  ~: Nothing ~=? (stringToGender "")
                  ]

main :: IO Counts
main = do
  runTestTT test2


