module Examples where

import           Checker
import           Data.Map as Map

exampleModel :: Model Int String
exampleModel = Mo {
    states = [1..3],
    actors = [Ag x | x <- [1..3]],
    erels = Map.fromList [(Ag 1, []), (Ag 2, [[1,2]]), (Ag 3, [[1,2,3]])],
    valuation = Map.fromList [("p", [1,2]),("q", [1])]
  }

exampleModel2 :: Model Int String
exampleModel2 = Mo {
    states = [1,2],
    actors = [Ag 1],
    erels = Map.fromList [(Ag 1, [[1,2]])],
    valuation = Map.fromList [("p", [1])]
}

exampleModel3 :: Model Int String
exampleModel3 = Mo {
    states = [1,2,3,4],
    actors = [Ag 1, Ag 2, Ag 3],
    erels = Map.fromList [(Ag 1, [[1,2], [3,4]]), (Ag 2, [[1,3],[2,4]]), (Ag 3, [[1,2,3,4]])],
    valuation = Map.fromList [("p", [1,2]), ("q", [1,3])]
}
