import           Checker
import           Data.Map   as Map
import           Examples
import           Test.HUnit

main :: IO Counts
main = Test.HUnit.runTestTT tests

tests :: Test
tests = TestList [testKOperators, testAnnouncements, testModelUpdate]

testKOperators :: Test
testKOperators = TestCase (assertEqual "Knows-operator is broken" True result) where
  result = check exampleModel 1 formula where
    formula = Knows (Ag 1) (Prop "p")

testAnnouncements :: Test
testAnnouncements = TestCase (assertEqual "Announcements are broken" True result) where
  result = check exampleModel2 1 formula where
    formula = Announce (Prop "p") (Knows (Ag 1) (Prop "p"))

testModelUpdate :: Test
testModelUpdate =
  TestCase (assertEqual "Updates should be true if the announcement is untrue in the present state" True result) where
    result = check exampleModel2 2 formula where
      formula = Announce (Prop "p") (Prop "p")
