module Base where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

data Formula p =
    Prop p                            |        Neg  (Formula p)
  | Conj (Formula p) (Formula p)      |        Disj (Formula p) (Formula p)
  | Knows Agent (Formula p)           |        Announce (Formula p) (Formula p)
  | GroupAnnounce [Agent] (Formula p)

newtype Agent = Ag Int deriving (Eq, Ord)
instance Show Agent where
  show (Ag n) = "a" ++ show n

type ErelMap state = Map.Map Agent [[state]]
-- ErelMaps are maps that bind agents to an equivalence relation

data Model state prop = Mo {
      states      :: [state],
      actors      :: [Agent],
      props       :: [prop],
      erels       :: ErelMap state,
      valuation   :: prop -> [state]
}
-- TODO Represent the knowledge of each Agent

instance (Show state, Show prop) => Show (Model state prop) where
  show (Mo states actors props erels valuation) =
        "{ States: "              ++ show states ++ " "  ++
        "Actors: "                ++ show actors ++ " "  ++
        "Propositions:"           ++ show props  ++ " "  ++
        "Equivalence relations:"  ++ show erels  ++ " "  ++
        "Valuations: "            ++ valuations  ++ " }" where
          valuations = show $ map (\prop -> show prop ++ " -> " ++ show (valuation prop)) props


-- Each prop is tied to a list of states where the prop holds
-- TODO Represent the knowledge of each Agent
-- TODO Fix quotes around each state when the valuation function is printed

check :: (Eq state) => Model state a -> state -> Formula a -> Bool
check model state (Prop prop) = state `elem` valuation model prop
check model state (Neg formula) = not $ check model state formula
check model state (Conj formula1 formula2) = check model state formula1 &&
                                             check model state formula2
check model state (Disj formula1 formula2) = check model state formula1 ||
                                             check model state formula2




exampleModel :: Model Int String
exampleModel = Mo {
    states = [1..3],
    actors = [Ag x | x <- [1..3]],
    props = ["p", "q"],
    erels = exampleErels,
    valuation = exampleValuation
  }

exampleErels :: ErelMap Int
exampleErels = Map.fromList [(Ag 1, []), (Ag 2, [[1,2]]), (Ag 3, [[1,2,3]])]

exampleValuation :: String -> [Int]
exampleValuation "p" = [1,2]
exampleValuation "q" = [1]
exampleValuation _   = []
