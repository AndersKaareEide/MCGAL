import           Control.Monad
import qualified Checker
import           Examples
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (empty)
import qualified Graphics.UI.Threepenny.SVG  as SVG
import qualified Text.Read as Text
import           Data.Maybe (fromJust)
import           Data.Map (Map, fromList, toList, insert, empty, (!))
import           Data.List (findIndex)
import           Data.IORef

import Paths
import Types


main :: IO ()
main = do
  path <- Paths.getStaticDir
  startGUI defaultConfig {
    jsCustomHTML = Just "index.html"
  , jsStatic = Just path
  , jsPort = Just 8024
    } setup

setup :: Window -> UI ()
setup w = do
    -- If TPG actually let you read the values of attributes this wouldn't
    -- be neccessary. Thanks for making read ops return () TPG.
    let startPos = (200,200)
    let elDragDims = (50,50)

    -- active elements
    elBody   <- UI.getBody w
    elRemove <- UI.button # set UI.text "Clear"

    elCanvas <- SVG.svg # set SVG.height "500"
                        # set SVG.width  "500"
                        # set style [("height", "500px"), ("width", "100%"),
                                     ("border-style", "solid")]
    elCanvDiv <- UI.div # set style [("height", "500px"), ("width", "100%")]
                        #+ [element elCanvas]

    elDrawCheck <- UI.input # set UI.type_ "checkbox"
    elCheckText <- UI.span # set UI.text "Line drawing mode"


    let initialState = AppState w empty empty StateMode Nothing Nothing "s1"
    stateRef <- liftIO $ newIORef initialState :: UI (IORef AppState)

    -- functionality
    let
      -- Function responsible for updating app state based on AppEvents
      update :: AppEvent -> UI ()
      update event = do
        state <- liftIO $ readIORef stateRef
        updState <- case event of
          MouseClick pos -> handleMClick pos state
          ClearCanvas -> do
            let state' = state { states = empty, edges = empty }
            redoLayout state'
            return state'
          StateMDown element -> handleMouseDown state element
          StateMUp element -> handleMouseUp state element
          MovState pos -> moveStateElem state pos
          UpdClickMode clickMode' -> UI.pure state { clickMode = clickMode'}
        liftIO $ writeIORef stateRef updState
        -- redoLayout updState

      redoLayout :: AppState -> UI ()
      redoLayout state = void $ do
        let stateList = map (Types.elem . snd) $ toList (states state) :: [Element]
        let edgeList = map (Types.lineElem . snd) $ toList (edges state) :: [Element]
        element elCanvas # set children edgeList
        element elCanvDiv # set children (elCanvas : stateList) -- TODO Render lines / edges
        element elBody # set children (elCanvDiv : [elRemove, elDrawCheck, elCheckText])


      handleMouseDown :: AppState -> Element -> UI AppState
      handleMouseDown state element =
        case clickMode state of
          StateMode -> UI.pure state { dragRef = Just element }
          LineMode  -> do
            refValue <- lookupElemData state element
            UI.pure state { tempRef = Just refValue } where

      handleMouseUp :: AppState -> Element -> UI AppState
      handleMouseUp state element =
        case clickMode state of
          StateMode -> UI.pure state { dragRef = Nothing }
          LineMode -> do
            let parent1 = fromJust $ tempRef state
            parent2 <- lookupElemData state element
            lineData <- mkSVGLine parent1 parent2
            let state' = state {
              edges = insert (lineId lineData) lineData (edges state),
              states = insertLineData (states state) parent1 parent2 lineData
            }
            redoLayout state'
            return state'

      handleMClick :: Pos -> AppState -> UI AppState
      handleMClick position state =
        case clickMode state of
          StateMode -> do
            state' <- addState state position
            redoLayout state' -- TODO Find smart way of updating layout without redrawing entire DOM
            return state'
          LineMode -> return state -- Do nothing
        -- return state {nxtStID = nextID', states = insert nextID inputData (states state)}


      addState :: AppState -> Pos -> UI AppState
      addState state pos = do
        let nextID = nxtStID state
        elBox <- UI.div #+ [UI.input]
                        #. "state"
                        # set style (mkPosAttr pos)
                        # set value nextID
                        # set UI.id_ nextID
        handleMClickListener elBox
        let inputData = ElemData nextID elBox pos []
        let nextID' = getNextID (nxtStID state)
        return state {nxtStID = nextID', states = insert nextID inputData (states state)}

      handleMClickListener :: Element -> UI ()
      handleMClickListener element = do
        -- TODO Eat event and do nothing if a child element like the input box was clicked
        --      rather than the state itself
        on UI.mousedown element $ const $ update $ StateMDown element
        on UI.mouseup element $ const $ update $ StateMUp element
        on UI.mousemove elBody $ \pos -> update $ MovState pos -- TODO Check if dragRef is Nothing

    -- TODO Crop elements that fit outside the canvas
    on UI.mousedown elCanvas $ \pos -> update $ MouseClick pos
    on UI.click elRemove $ const $ update ClearCanvas


    on UI.checkedChange elDrawCheck $ \checked ->
      let newMode = if checked
                    then LineMode
                    else StateMode
      in update $ UpdClickMode newMode


    -- Run setup functions
    UI.addStyleSheet w "default.css"
    redoLayout initialState


-- Moves state to the given position and updates all edges
-- tied to this state
moveStateElem :: AppState -> Pos -> UI AppState
moveStateElem state pos@(x,y) =
  case dragRef state of
    Nothing -> UI.pure state
    Just element -> do
      let styling = [("left", show x ++ "px"), ("top", show y ++ "px")]
      elemData <- lookupElemData state element :: UI ElemData
      element' <- UI.element element # set style styling
      let elemData' = elemData { Types.elem = element', Types.pos = pos }
      let state' = state { states = insert (elemId elemData') elemData' (states state)}
      movStateEdges state' elemData pos

-- Function that based on a state, and a position, updates the position of all
-- edges connected to that state
movStateEdges :: AppState -> ElemData -> Pos -> UI AppState
movStateEdges state elemData pos = do
  let edges = Types.edges state       :: Map String LineData
  let edgeIDs = Types.edgeIDs elemData :: [String]
  let edges' = map (\id_ -> (!) edges id_ ) edgeIDs :: [LineData]
  let updTups = map (\edge -> findStartOrEnd edge (elemId elemData)) edges' :: [(LineData, Int)]
  movedEdges <- mapM (\tuple -> movStateEdge tuple pos) updTups :: UI [LineData]
  let edges'' = foldr (\lData acc -> insert (lineId lData) lData acc) edges movedEdges :: Map String LineData
  return state { Types.edges = edges'' }


movStateEdge :: (LineData, Int) -> Pos -> UI LineData
movStateEdge (line, posNum) (newX, newY) = do
  elem' <- case posNum of
    1 -> (element (lineElem line)) # set SVG.x1 (show newX)
                      # set SVG.y1 (show newY)
    2 -> (element (lineElem line)) # set SVG.x2 (show newX)
                      # set SVG.y2 (show newY)
  return line { lineElem = elem' }


findStartOrEnd :: LineData -> String -> (LineData, Int)
findStartOrEnd edge parID =
  if (fst $ parents edge) == parID
    then (edge, 1)
    else (edge, 2)

-- Makes top and left style attributes with the input position values
mkPosAttr :: Pos -> [(String, String)]
mkPosAttr (x,y) = [("left", show x ++ "px"),
                   ("top", show y ++ "px")]

-- Calculates the new position of an element based on the position of the mouse,
-- the element's current position and its dimensions
calcRelPos :: Pos -> Pos -> Pos -> Pos
calcRelPos mousePos oldPos dims =
  (fst mousePos + (fst oldPos - fst dims `div` 2), snd mousePos + (snd oldPos - snd dims `div` 2))

-- Generates the ID of the next state by incrementing the old stateID by 1
getNextID :: String -> String
getNextID (_:stateNum) = "s" ++ show nextNum where
  nextNum = 1 + read stateNum :: Int

-- Convenience function for finding elements from their ID
lookupElemData :: AppState -> Element -> UI ElemData
lookupElemData state element = do
  elemid <- element # get value
  let elemMap = states state
  UI.pure $ elemMap ! elemid

-- Creates a new SVG line between the two input elements and
-- stores a reference to it in linesRef
mkSVGLine :: ElemData -> ElemData -> UI LineData
mkSVGLine strtNode endNode = do
    lineElem <- SVG.line # set SVG.x1 (show $ fst $ pos strtNode)
                         # set SVG.y1 (show $ snd $ pos strtNode)
                         # set SVG.x2 (show $ fst $ pos endNode)
                         # set SVG.y2 (show $ snd $ pos endNode)
                         #. "edge"
    let lineID = elemId strtNode ++ elemId endNode
    UI.pure $ LineData lineID lineElem (elemId strtNode, elemId endNode)

-- Utility function for inserting LineData into ElemData to keep
-- track of which states have edges connected to them
insertLineData :: Map String ElemData -> ElemData -> ElemData ->
                  LineData -> Map String ElemData
insertLineData elemMap par1 par2 edge =
  insert (elemId par2) (par2 { edgeIDs = edgeID:(edgeIDs par2) }) elemMap' where
    elemMap' = insert (elemId par1) (par1 { edgeIDs = edgeID:(edgeIDs par1) }) elemMap
    edgeID = lineId edge

onClick :: WriteAttr Element String
onClick = UI.mkWriteAttr $ \el s ->
    runFunction $ ffi "$(%1).on('mousedown', (alert(%2))" el s
