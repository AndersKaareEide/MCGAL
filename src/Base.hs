module Base where

data Formula p =
    Prop p                          |        Neg  (Formula p)
  | Conj (Formula p) (Formula p)    |        Disj (Formula p) (Formula p)
-- TODO Expand with complete list of symbols

newtype Agent = Ag Int deriving (Eq, Ord)
instance Show Agent where
  show (Ag n) = "a" ++ show n

data Model state prop = Mo {
      states      :: [state],
      actors      :: [Agent],
      props       :: [prop],
      valuation   :: prop -> [state]
}
-- TODO Represent the knowledge of each Agent

instance (Show state, Show prop) => Show (Model state prop) where
  show (Mo states actors props valuation) =
        "{ States: "     ++ show states ++ " "  ++
        "Actors: "       ++ show actors ++ " "  ++
        "Propositions:"  ++ show props  ++ " "  ++
        "Valuations: "   ++ valuations  ++ " }" where
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
    valuation = exampleValuation
  }

exampleValuation :: String -> [Int]
exampleValuation "p" = [1,2]
exampleValuation "q" = [1]
exampleValuation _   = []
