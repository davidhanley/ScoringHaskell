import Test.HUnit

import Scoring

test2 :: Test
test2 = TestCase (assertEqual "test2" (stringToGender "Male") (Just Male) )

tests :: Test
tests = TestList [TestLabel "test1" test2]

main :: IO Counts
main = do
  runTestTT tests


