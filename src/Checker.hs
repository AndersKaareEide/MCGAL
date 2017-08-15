module Checker where

import qualified Data.Map    as Map
import qualified Data.Set    as Set
import           Data.List
import           Debug.Trace

infixr 5 |=
(|=) :: (Ord state, Show state, Ord prop) =>
  (Model state prop, state) -> Formula prop -> Bool
(model, state) |= form = check model state form


data Formula p =
    Prop p                            |        Neg  (Formula p)
  | Conj (Formula p) (Formula p)      |        Disj (Formula p) (Formula p)
  | Knows Agent (Formula p)           |        Announce (Formula p) (Formula p)
  | GroupAnnounce [Agent] (Formula p) deriving (Eq)

p, q, r ,s :: Formula String
p = Prop "p"; q = Prop "q"
r = Prop "r"; s = Prop "s"

newtype Agent = Ag Int deriving (Eq, Ord)
instance Show Agent where
  show (Ag n) = "a" ++ show n

type ErelMap state = Map.Map Agent [[state]]
-- ErelMaps are maps that bind agents to an equivalence relation

data Model state prop = Mo {
      states    :: [state],
      actors    :: [Agent],
      erels     :: ErelMap state,
      valuation :: Map.Map prop [state]
}

instance (Show state, Show prop) => Show (Model state prop) where
  show (Mo states actors erels valuation) =
        "{ States: "              ++ show states ++ " "  ++
        "Actors: "                ++ show actors ++ " "  ++
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
  (GroupAnnounce _ _) -> checkGroupAnnouncement model state formula
-- If not M,s |= announcement then the formula is true

checkGroupAnnouncement :: (Show state, Ord state, Ord p) => Model state p -> state -> Formula p -> Bool
checkGroupAnnouncement model state formula =
  case formula of
  GroupAnnounce [] formula2 -> check model state formula2         -- Empty group is powerless
  GroupAnnounce _ formula2@(Prop _) -> check model state formula2  -- Atomic permanence
  GroupAnnounce agents formula2
    | formEval -> True                                -- If formula2 is already true, agents simply announce Top
    | not $ containsKnOp formula2 -> formEval         -- If formula2 contains no K ops, valuation cannot change through announcements
    | otherwise -> check updatedModel state formula2  -- Extract atoms in formula2 and test announcements which could change formula2's truth value
    where
      formEval = check model state formula2
      atoms = extractProps formula2
      model' = poolGroupKnowledge model state agents -- Creates a model to verify what the group announce based on distributed knowledge
      announcements = genAnnouncementSet model' state atoms -- Generates actual announcement for the group based on their knowledge of relevant props
      updatedModel = foldl updateModel model announcements --upds_pa model announcements
  _ -> error "checkGroupAnnouncement called without group announcement"

-- TODO Verify that this shit is even remotely sane

updateModel :: (Show state, Ord state, Eq state, Ord prop) =>
                Model state prop -> Formula prop -> Model state prop
updateModel model@(Mo states actors erels valuation) announcement =
  Mo states' actors erels' valuation where
  states' = filter (\x -> check model x announcement) states
  erels' = Map.map (filter (\list -> length list > 1)) temp
  temp = Map.map (map (filter (`elem` states'))) erels

-- TODO Find out if I need to update valuation function
-- Is there any reason to remove singleton lists in the epistemic relations?

containsKnOp :: Formula a -> Bool
containsKnOp form =
  case form of
    Prop _ -> False
    Neg form' -> containsKnOp form'
    Disj formula1 formula2 -> any containsKnOp [formula1, formula2]
    Conj formula1 formula2 -> any containsKnOp [formula1, formula2]
    Announce announcement form' -> containsKnOp announcement || containsKnOp form'
    GroupAnnounce _ form' -> containsKnOp form' -- GroupPub should probably be treated as having
    Knows _ _ -> True                         -- announcements bound to each agent by now


-- Function which extracts the set of all atoms a formula consists of
extractProps :: (Eq a) => Formula a -> [Formula a]
extractProps form =
  nub $ extractProps' form


extractProps' :: Formula a -> [Formula a]
extractProps' form =
  case form of
    Prop _ -> [form]
    Neg form' -> extractProps' form'
    Disj formula1 formula2 ->
      foldr (\form' props -> extractProps' form' ++ props) [] [formula1, formula2]
    Conj formula1 formula2 ->
      foldr (\form' props -> extractProps' form' ++ props) [] [formula1, formula2]
    Knows _ form' -> extractProps' form'
    Announce announcement form' -> extractProps' announcement ++ extractProps' form'
    GroupAnnounce agents form' -> extractProps' form'

-- Function which checks if a group can publicly announce a formula, that is
-- if the group has distributed knowledge of the formula being true
canAnnounce :: (Ord state, Show state, Ord prop) =>
                Model state prop -> state -> Agent -> Formula prop -> Bool
canAnnounce model state groupAgent form =
  check model state (Knows groupAgent form)


-- Function for generating a set of public announcements for a set of agents based
-- on a set of atomic propositions, in order to convey as much info as possible
genAnnouncementSet :: (Ord state, Show state, Ord prop) =>
                       Model state prop -> state -> [Formula prop] -> [Formula prop]
genAnnouncementSet model state =
  filter (canAnnounce model state (Ag $ -1))

-- Generates intersection of equivalence relations of all agents in a group,
-- by creating a new model where 'agent' -1 has the combined knowledge of the group
poolGroupKnowledge :: (Eq state) => Model state prop -> state -> [Agent] -> Model state prop
poolGroupKnowledge model@(Mo states actors erels valuations) actualState coalition = model' where
  allRels = concatMap (\agent -> Map.findWithDefault [] agent erels) coalition
  relevantRels = filter (actualState `elem`) allRels
  distributedRels = [foldl1 intersect relevantRels]
  model' = model {erels = Map.fromList [(Ag $ -1, distributedRels)]}

-- Concatinate all relations belonging to agents in coalition
-- Filter all sets not containing the actual state
-- Take intersection of all remaining sets
