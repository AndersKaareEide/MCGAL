module Checker where

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import           Debug.Trace

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
      states    :: [state],
      actors    :: [Agent],
      props     :: [prop],
      erels     :: ErelMap state,
      valuation :: Map.Map prop [state]
}

instance (Show state, Show prop) => Show (Model state prop) where
  show (Mo states actors props erels valuation) =
        "{ States: "              ++ show states ++ " "  ++
        "Actors: "                ++ show actors ++ " "  ++
        "Propositions:"           ++ show props  ++ " "  ++
        "Equivalence relations:"  ++ show erels  ++ " "  ++
        "Valuations: "            ++ show valuation  ++ " }"


check :: (Eq state, Ord state, Show state, Ord p) =>
          Model state p -> state -> Formula p -> Bool
check model state formula = case formula of
  (Prop prop) -> case Map.lookup prop (valuation model) of
      Just states -> state `elem` states
      Nothing     -> False
  (Neg formula) -> not $ check model state formula
  (Conj formula1 formula2) -> check model state formula1 &&
                              check model state formula2
  (Disj formula1 formula2) -> check model state formula1 ||
                              check model state formula2
  (Knows agent formula) -> case Map.lookup agent (erels model) of
      Just [] -> check model state formula -- Agent has complete knowledge, can distinguish any states
      Just stateSets -> all (\x -> check model x formula) indishtinguableStates where
        indishtinguableStates = Set.fromList (concat (filter (elem state) stateSets))
      Nothing -> error ("Model does not contain " ++ show agent)
  (Announce announcement formula) -> not (check model state announcement) ||
                                     check updatedModel state formula where
      updatedModel = updateModel model announcement
-- If not M,s |= announcement then the formula is true

updateModel :: (Show state, Ord state, Eq state, Ord prop) =>
                Model state prop -> Formula prop -> Model state prop
updateModel model@(Mo states actors props erels valuation) announcement =
  Mo states' actors props erels' valuation where
  states' = filter (\x -> check model x announcement) states
  erels' = Map.map (filter (\list -> length list > 1)) temp
  temp = Map.map (map (filter (`elem` states'))) erels

-- TODO Find out if I need to update valuation function
-- Is there any reason to remove singleton lists in the epistemic relations?
