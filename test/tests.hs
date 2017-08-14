import Test.HUnit
import Data.Map as Map
import Checker

main :: IO Counts
main = Test.HUnit.runTestTT tests

tests :: Test
tests = TestList [testKOperators, testAnnouncements]

testKOperators :: Test
testKOperators = TestCase (assertEqual "Knows-operator is broken" True result) where
  result = check exampleModel 1 formula where
    formula = Knows (Ag 1) (Prop "p")

testAnnouncements :: Test
testAnnouncements = TestCase (assertEqual "Announcements are broken" True result) where
  result = check exampleModel2 1 formula where
    formula = Announce (Prop "p") (Knows (Ag 1) (Prop "p"))


exampleModel :: Model Int String
exampleModel = Mo {
    states = [1..3],
    actors = [Ag x | x <- [1..3]],
    props = ["p", "q"],
    erels = Map.fromList [(Ag 1, []), (Ag 2, [[1,2]]), (Ag 3, [[1,2,3]])],
    valuation = Map.fromList [("p", [1,2]),("q", [1])]
  }

exampleModel2 :: Model Int String
exampleModel2 = Mo {
    states = [1,2],
    actors = [Ag 1],
    props = ["p"],
    erels = Map.fromList [(Ag 1, [[1,2]])],
    valuation = Map.fromList [("p", [1])]
}
