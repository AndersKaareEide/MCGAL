import           Control.Monad


import           Checker
import           Examples
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
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


        -- active elements
        elBody   <- UI.getBody w
        elRemove <- UI.button # set UI.text "Clear"
        elCanvas <- UI.div # set style [("height", "500px"), ("width", "100%"),
                                        ("border-style", "solid")]

        elParent <- UI.div #  set style [("height", "100px"), ("width", "100px"),
                                        ("border-style", "solid")]
        elChild <- UI.button # set text "Child"

        element elParent #+ [element elChild]

        eventsRef   <- liftIO $ newIORef [] :: UI (IORef [(String, IO ())])
        draggableRef <- liftIO $ newIORef (Nothing, startPos) :: UI (IORef (Maybe Element,(Int, Int)))
        inputs      <- liftIO $ newIORef [] :: UI (IORef [Element])


        -- functionality
        let
            redoLayout :: UI ()
            redoLayout = void $ do
                layout <- mkLayout =<< liftIO (readIORef inputs)
                let body = getBody w
                body # set children  (elCanvas : [layout])
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
                liftIO $ modifyIORef inputs (elBox:)
                where
                  boxStyle = getDraggableStyle ++ mkPosAttr position
                  inputStyle = [("width", "80%")]

            clearCanvas :: UI ()
            clearCanvas = liftIO $ writeIORef inputs []

            -- Adds event listeners to the element to allow it to be
            -- dragged around on the canvas
            makeDraggable :: Element -> UI ()
            makeDraggable element = do
              -- TODO Eat event and do nothing if a child element was clicked
              --      rather than the state itself
              on UI.mousedown element $ const $ void $
                liftIO $ modifyIORef draggableRef (\(_, pos) -> (Just element, pos))

              on UI.mouseup elBody $ const $ void $
                liftIO $ modifyIORef draggableRef (\(_, pos) -> (Nothing, pos))

              on UI.mousemove elBody $ \mousePos -> do
                dragging <- liftIO $ fst <$> readIORef draggableRef
                case dragging of
                  Just element -> do
                    oldPos <- snd <$> liftIO (readIORef draggableRef)
                    void $ liftIO $ modifyIORef draggableRef (\(bool, _) -> (bool, mousePos))
                    moveElement element mousePos
                  _ ->
                    UI.pure () -- Do nothing

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


-- Calculates the new position of an element based on the position of the mouse,
-- the element's current position and its dimensions
calcRelPos :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
calcRelPos mousePos oldPos dims =
  (fst mousePos + (fst oldPos - fst dims `div` 2), snd mousePos + (snd oldPos - snd dims `div` 2))
