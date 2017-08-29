module Types where

import Graphics.UI.Threepenny

data ClickMode = StateMode
  | LineMode

type Pos = (Int, Int)

data ElemData = ElemData {
  elemId    :: String,
  elem      :: Element,
  pos       :: Pos,
  edges     :: [String] -- lines is the IDs of each line this elem is connected to
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
