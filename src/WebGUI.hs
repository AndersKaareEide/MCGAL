import           Control.Monad
import           Checker
import           Examples
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
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

    -- IORef structure for updating SVG lines
    -- Redraw line every time one of its "parents" are moved
    -- (Element, Pos, Pos)

    -- State element needs to which SVG lines it is connected to
    -- and if it is the start or end point for each line

    -- Model creator needs to know of all state elements and the
    -- lines between them as well as the value of the text fields
    -- within each state element

    -- Click listener for "canvas" needs to know if the user is drawing
    -- states or lines


    -- active elements
    elBody   <- UI.getBody w
    elRemove <- UI.button # set UI.text "Clear"
    elCanvas <- SVG.svg # set SVG.height "500"
                        # set SVG.width  "500"
                        # set style [("height", "500px"), ("width", "100%"),
                                     ("border-style", "solid")]

    elParent <- UI.div #  set style [("height", "100px"), ("width", "100px"),
                                    ("border-style", "solid")]
    elChild <- UI.button # set text "Child" # set UI.id_ "child"
    elDrawCheck <- UI.input # set UI.type_ "checkbox"
    elCheckText <- UI.span # set UI.text "Line drawing mode"


    element elParent #+ [element elChild]

    -- TODO Refactor these into a state object?
    eventsRef    <- liftIO $ newIORef [] :: UI (IORef [(String, IO ())])
    draggableRef <- liftIO $ newIORef Nothing :: UI (IORef (Maybe Element))    -- Used to keep track of which element the user is dragging around
    tempLineRef  <- liftIO $ newIORef Nothing :: UI (IORef (Maybe ElemData))   -- Temporarily stores one of the 'parents' while the user is drawing lines

    stateIDs     <- liftIO $ newIORef ["s" ++ show x | x <- [1..]] :: UI (IORef [String]) -- Used to make IDs for each new state

    statesRef    <- liftIO $ newIORef Data.Map.empty :: UI (IORef (Map String ElemData))  -- Map of the id of state elements and data about them
    linesRef     <- liftIO $ newIORef Data.Map.empty :: UI (IORef (Map String LineData))  -- Map of the id of line elements and data about their parents
    drawingRef   <- liftIO $ newIORef StateMode :: UI (IORef ClickMode)

    -- functionality
    let
        redoLayout :: UI ()
        redoLayout = void $ do
          let states = map (Types.elem . snd) . toList <$> readIORef statesRef
          let body = getBody w
          layout <- mkLayout =<< liftIO states
          body # set children  (elCanvas : [layout, elDrawCheck, elCheckText])

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
          [row [element elRemove]]
          ++ map element xs ++
          [element elParent]


        addState :: Pos ->  UI ()
        addState position = do
          newID <- liftIO $ getNextID stateIDs
          elInput <- UI.input # set style inputStyle
          elBox <- UI.div # set style boxStyle #+ [UI.element elInput]
                          # set value newID
          addStateListener elBox
          let inputData = ElemData newID elBox position []
          liftIO $ modifyIORef statesRef (insert newID inputData)
          where
            boxStyle = getDraggableStyle ++ mkPosAttr position
            inputStyle = [("width", "80%")]

        clearCanvas :: UI ()
        clearCanvas = liftIO $ writeIORef statesRef Data.Map.empty

        addStateListener :: Element -> UI ()
        addStateListener element = do
          -- TODO Eat event and do nothing if a child element like the input box was clicked
          --      rather than the state itself
          on UI.mousedown element $ \pos -> do
            clickMode <- liftIO $ readIORef drawingRef
            case clickMode of
              StateMode -> void $ liftIO $ writeIORef draggableRef (Just element)
              LineMode -> do
                refValue <- getElemFromRef element statesRef
                liftIO $ writeIORef tempLineRef $ Just refValue

          -- TODO Somehow find position of elements when drawing line
          on UI.mouseup element $ const $ do
            clickMode <- liftIO $ readIORef drawingRef
            case clickMode of
              StateMode -> void $ liftIO $ writeIORef draggableRef Nothing
              LineMode -> do -- TODO Find out if I need to check what kind of element the user 'drops' on
                elemid <- element # get value
                parent1 <- liftIO $ fromJust <$> readIORef tempLineRef :: UI ElemData
                parent2 <- getElemFromRef element statesRef            :: UI ElemData
                lineElem <- mkSVGLine parent1 parent2
                let lineID = elemId parent1 ++ elemId parent2
                let lineData = LineData lineID lineElem (elemId parent1, elemId parent2)
                void $ UI.element elCanvas #+ [UI.element lineElem] -- TODO Find better solution so I can remove lines again
                liftIO $ modifyIORef linesRef (insert lineID lineData)
                liftIO $ modifyIORef statesRef (insert (elemId parent1) (parent1 {edges = lineID : edges parent1} ) )
                liftIO $ modifyIORef statesRef (insert (elemId parent1) (parent2 {edges = lineID : edges parent2} ) )
                UI.element elCanvas #+ [UI.element lineElem]
                redoLayout

          on UI.mousemove elBody $ \mousePos -> do
            clickMode <- liftIO $ readIORef drawingRef
            case clickMode of
              StateMode -> do
                dragging <- liftIO $ readIORef draggableRef
                case dragging of
                  Just element ->
                    moveStateElem element mousePos
                  _ ->
                    UI.pure () -- No element selected, do nothing
              LineMode ->
                    UI.pure () -- Do nothing

                -- 1. Check if drawing lines
                -- 2. Check if selected start point
                -- 3. On mouseup check if dropping on valid end point
                -- 4. If 3., draw actual line on canvas
                -- 5. Store reference to drawn line and update start / end points
                --    if either of them are moved by the user

    -- Event listeners

    -- TODO Crop elements that fit outside the canvas
    on UI.mousedown elCanvas $ \pos -> addState pos >> redoLayout
    on UI.click elRemove $ \_ -> clearCanvas >> redoLayout

    on UI.mouseup elBody $ const $ do
      clickMode <- liftIO $ readIORef drawingRef
      case clickMode of
        StateMode -> void $ liftIO $ writeIORef draggableRef Nothing
        LineMode -> liftIO $ liftIO $ writeIORef tempLineRef Nothing -- Line drawing failed, clear reference

    -- Try stopping event propagation by storing each event in an IORef
    -- and only dispatch event from parent if the child element has not
    -- already fired one
    -- TODO Make generic system that automagically checks for events from
    -- child elements before handling event
    on UI.click elParent $ \event -> do
      events <- liftIO $ readIORef eventsRef
      if "child" `notElem` map fst events
        then do
          liftIO $ modifyIORef eventsRef (("parent", print "parent") :)
          liftIO $ handleEvents eventsRef
        else
          liftIO $ writeIORef eventsRef []
          -- Clear events if event from elChild is found

    on UI.click elChild $ \_ -> do
      void $ liftIO $ modifyIORef eventsRef (("child", print "child") :)
      liftIO $ handleEvents eventsRef

    on UI.checkedChange elDrawCheck $ \checked ->
      let newMode = if checked
                    then LineMode
                    else StateMode
      in void $ liftIO $ writeIORef drawingRef newMode



    -- Run setup functions
    redoLayout

handleEvents :: IORef [(String, IO ())] -> IO ()
handleEvents ref = do
  list <- readIORef ref
  snd . head $ list

-- Moves state to the given position and updates all edges
-- tied to this state
moveStateElem :: Element -> Pos -> UI ()
moveStateElem element (x,y) = do
  let pos = [("left", show x ++ "px"), ("top", show y ++ "px")]
  void $ UI.pure element # set style (getDraggableStyle ++ pos)
-- TODO move edges as well


-- Returns style tuples for Draggable elements
getDraggableStyle :: [(String, String)]
getDraggableStyle = [("height", "50px"), ("width", "50px"),
                    ("border-style", "solid"),
                    ("position", "absolute"),
                    ("background", "blue")]

-- Makes top and left style attributes with the input position values
mkPosAttr :: Pos -> [(String, String)]
mkPosAttr (x,y) = [("left", show x ++ "px"),
                   ("top", show y ++ "px")]



-- Calculates the new position of an element based on the position of the mouse,
-- the element's current position and its dimensions
calcRelPos :: Pos -> Pos -> Pos -> Pos
calcRelPos mousePos oldPos dims =
  (fst mousePos + (fst oldPos - fst dims `div` 2), snd mousePos + (snd oldPos - snd dims `div` 2))

-- Horrible mess of an IORef based counter to generate IDs for state elements
getNextID :: IORef [String] -> IO String
getNextID ref = do
  ids <- readIORef ref :: IO [String]
  writeIORef ref (drop 1 ids)
  return (head ids)

-- Convenience function for looking up data about elements from an IORef
-- based on their id, which is stored in the 'value' attribute of the element
-- since that is the only attribute TPG allows you to read the value of
getElemFromRef :: Element -> IORef (Map String ElemData) -> UI ElemData
getElemFromRef element ref = do
  elemid  <- element # get value
  elemMap <- liftIO $ readIORef ref
  UI.pure $ elemMap ! elemid

-- Creates a new SVG line between the two input elements and
-- stores a reference to it in linesRef
mkSVGLine :: ElemData -> ElemData -> UI Element
mkSVGLine strtNode endNode =
   SVG.line # set SVG.x1 (show $ fst $ pos strtNode)
            # set SVG.y1 (show $ snd $ pos strtNode)
            # set SVG.x2 (show $ fst $ pos endNode)
            # set SVG.y2 (show $ snd $ pos endNode)
            # set SVG.stroke "blue"
            # set SVG.stroke_width "5"


-- State and Event notes
-- Event types:
--  AddState Pos
--  MovState (Elem, Pos)
--
