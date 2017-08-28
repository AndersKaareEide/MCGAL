import           Control.Monad
import           Checker
import           Examples
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG
import qualified Text.Read as Text
import           Data.Maybe (fromJust)
import           Data.Map (fromList)
import           Data.IORef

import Paths

data State = State {
  mStates :: [Element],
  model :: Model Int String
}

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
    -- (Element, (Int, Int), (Int, Int))

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
    elCanvas <- getSVGtestElem # set style [("height", "500px"), ("width", "100%"),
                                            ("border-style", "solid")]

    elParent <- UI.div #  set style [("height", "100px"), ("width", "100px"),
                                    ("border-style", "solid")]
    elChild <- UI.button # set text "Child" # set UI.id_ "child"
    elDrawCheck <- UI.input # set UI.type_ "checkbox"
    elCheckText <- UI.span # set UI.text "Line drawing mode"


    element elParent #+ [element elChild]


    eventsRef    <- liftIO $ newIORef [] :: UI (IORef [(String, IO ())])
    draggableRef <- liftIO $ newIORef Nothing :: UI (IORef (Maybe Element))

    statesRef    <- liftIO $ newIORef [] :: UI (IORef [Element])

    drawingRef   <- liftIO $ newIORef False :: UI (IORef Bool)

    -- functionality
    let
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef statesRef)
            let body = getBody w
            body # set children  (elCanvas : [layout, elDrawCheck, elCheckText])
            -- TODO Figure out why using mainDiv doesn't work

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elRemove]]
            ++ map element xs ++
            [element elParent]


        addInput :: (Int, Int) ->  UI ()
        addInput position = do
            let elInput = UI.input # set value "State"
                                   # set style inputStyle
            elBox <- UI.div # set style boxStyle #+ [elInput]
            makeDraggable elBox
            liftIO $ modifyIORef statesRef (elBox:)
            where
              boxStyle = getDraggableStyle ++ mkPosAttr position
              inputStyle = [("width", "80%")]

        clearCanvas :: UI ()
        clearCanvas = liftIO $ writeIORef statesRef []

        -- Adds event listeners to the element to allow it to be
        -- dragged around on the canvas
        makeDraggable :: Element -> UI ()
        makeDraggable element = do
          -- TODO Eat event and do nothing if a child element was clicked
          --      rather than the state itself
          on UI.mousedown element $ const $ void $
            liftIO $ writeIORef draggableRef (Just element)

          on UI.mouseup elBody $ const $ void $
            liftIO $ writeIORef draggableRef Nothing

          on UI.mousemove elBody $ \mousePos -> do
            dragging <- liftIO $ readIORef draggableRef
            case dragging of
              Just element ->
                moveElement element mousePos
              _ ->
                UI.pure () -- Do nothing

                -- 1. Check if drawing lines
                -- 2. Check if selected start point
                -- 3. On mouseup check if dropping on valid end point
                -- 4. If 3., draw actual line on canvas
                -- 5. Store reference to drawn line and update start / end points
                --    if either of them are moved by the user

    -- Event listeners

    -- TODO Crop elements that fit outside the canvas
    on UI.mousedown elCanvas $ \pos -> addInput pos >> redoLayout
    on UI.click elRemove $ \_ -> clearCanvas >> redoLayout

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
      void $ liftIO $ writeIORef drawingRef checked

    redoLayout

handleEvents :: IORef [(String, IO ())] -> IO ()
handleEvents ref = do
  list <- readIORef ref
  snd . head $ list

-- Moves element to the given position
moveElement :: Element -> (Int, Int) -> UI ()
moveElement element (x,y) = do
  let pos = [("left", show x ++ "px"), ("top", show y ++ "px")]
  void $ UI.pure element # set style (getDraggableStyle ++ pos)

-- Returns style tuples for Draggable elements
getDraggableStyle :: [(String, String)]
getDraggableStyle = [("height", "50px"), ("width", "50px"),
                    ("border-style", "solid"),
                    ("position", "absolute"),
                    ("background", "blue")]

-- Makes top and left style attributes with the input position values
mkPosAttr :: (Int, Int) -> [(String, String)]
mkPosAttr (x,y) = [("left", show x ++ "px"),
                   ("top", show y ++ "px")]

getSVGtestElem :: UI Element
getSVGtestElem =
  SVG.svg # set SVG.height "500"
          # set SVG.width  "500"
          #+ [line] where
            line = SVG.line
              # set SVG.x1 "50"
              # set SVG.y1 "50"
              # set SVG.x2 "450"
              # set SVG.y2 "450"
              # set SVG.stroke "blue"
              # set SVG.stroke_width "5"



-- Calculates the new position of an element based on the position of the mouse,
-- the element's current position and its dimensions
calcRelPos :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
calcRelPos mousePos oldPos dims =
  (fst mousePos + (fst oldPos - fst dims `div` 2), snd mousePos + (snd oldPos - snd dims `div` 2))
