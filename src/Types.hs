module Types where

import Graphics.UI.Threepenny
import Data.Map

data ClickMode = StateMode
  | LineMode

data AppEvent = MouseClick Pos
  | StateMDown Element
  | StateMUp Element
  | StateClick Element
  | MovState Pos
  | UpdClickMode ClickMode
  | ClearCanvas

type Pos = (Int, Int)

data AppState = AppState {
  window    :: Window,
  states    :: Map String ElemData, -- Map of the id of state elements and data about them
  edges     :: Map String LineData, -- Map of the id of line elements and data about their parents
  clickMode :: ClickMode,
  tempRef   :: Maybe ElemData,      -- Temporarily stores one of the 'parents' while the user is drawing lines
  dragRef   :: Maybe Element,       -- Used to keep track of which element the user is dragging around
  nxtStID   :: String               -- The ID for the next state
} -- TODO Maybe unify tempRef and dragRef?

data ElemData = ElemData {
  elemId    :: String,
  elem      :: Element,
  pos       :: Pos,
  edgeIDs   :: [String] -- lines is the IDs of each line this elem is connected to
}

data LineData = LineData {
  lineId    :: String,
  lineElem  :: Element,
  parents   :: (String, String) -- parents is the IDs of each 'parent' node
}

instance Eq ElemData where
  data1 == data2 = elemId data1 == elemId data2

instance Ord ElemData where
  data1 `compare` data2 = elemId data1 `compare` elemId data2

instance Eq LineData where
  line1 == line2 = lineId line1 == lineId line2

instance Ord LineData where
  line1 `compare` line2 = lineId line1 `compare` lineId line2
