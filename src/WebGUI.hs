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
        elRemove <- UI.button # set UI.text "Clear"
        elResult <- UI.span
        elCanvas <- UI.div # set style [("height", "500px"), ("width", "100%"),
                                        ("border-style", "solid")]

        elParent <- UI.div #  set style [("height", "100px"), ("width", "100px"),
                                        ("border-style", "solid")]
        elChild <- UI.button # set text "Child"
        elDragable <- UI.div # set style (getDragableStyle ++ mkPosAttr startPos)

        UI.pure elParent #+ [UI.pure elChild]

        eventsRef   <- liftIO $ newIORef [] :: UI (IORef [(String, IO ())])
        dragableRef <- liftIO $ newIORef (False, startPos) :: UI (IORef (Bool,(Int, Int)))
        inputs      <- liftIO $ newIORef [] :: UI (IORef [Element])


        -- functionality
        let
            displayTotal = void $ do
                xs <- mapM (get value) =<< liftIO (readIORef inputs)
                element elResult

            redoLayout :: UI ()
            redoLayout = void $ do
                layout <- mkLayout =<< liftIO (readIORef inputs)
                let body = getBody w
                let mainDiv = fromJust <$> getElementById w "mainDiv"
                mainDiv # set children  (elCanvas : [layout])
                -- TODO Figure out why div doesn't work
                displayTotal

            mkLayout :: [Element] -> UI Element
            mkLayout xs = column $
                [row [element elRemove]
                ,UI.hr]
                ++ map element xs ++
                [UI.hr
                ,row [UI.span # set text "Sum: ", element elResult]
                ,UI.pure elParent
                ,UI.pure elDragable
                ]

            addInput :: (Int, Int) ->  UI ()
            addInput position = do
                elBox <- UI.div # set style (styles position)
                liftIO $ modifyIORef inputs (elBox:)
                where
                  styles (x,y) = [("position", "absolute"), ("background", "red"),
                                  ("top", show y ++ "px"), ("left", show x ++ "px"),
                                  ("height", "50px"), ("width", "50px")]

            removeInput :: UI ()
            removeInput = liftIO $ writeIORef inputs []

        on UI.mousedown elCanvas $ \pos -> addInput pos   >> redoLayout
        on UI.click elRemove $ \_ -> removeInput >> redoLayout

        -- Try stopping event propagation by storing each event in an IORef
        -- and only dispatch event from parent if the child element has not
        -- already fired one

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

        -- Register that the user is about to drag
        on UI.mousedown elDragable $ const $ void $
          liftIO $ modifyIORef dragableRef (\(_, pos) -> (True, pos))

        on UI.mouseup elDragable $ const $ void $
          liftIO $ modifyIORef dragableRef (\(_, pos) -> (False, pos))

        on UI.mousemove elDragable $ \mousePos -> do
          dragging <- liftIO $ fst <$> readIORef dragableRef
          if dragging
            then do
              oldPos <- snd <$> liftIO (readIORef dragableRef)
              let newPos = calculatePos mousePos oldPos elDragDims
              void $ liftIO $ modifyIORef dragableRef (\(bool, _) -> (bool, newPos))
              moveElement elDragable newPos
            else
              UI.pure () -- Do nothing


        redoLayout

handleEvents :: IORef [(String, IO ())] -> IO ()
handleEvents ref = do
  list <- readIORef ref
  snd . head $ list

-- Makes top and left style attributes with the input position values
mkPosAttr :: (Int, Int) -> [(String, String)]
mkPosAttr (x,y) = [("left", show x ++ "px"),
                   ("top", show y ++ "px")]

moveElement :: Element -> (Int, Int) -> UI ()
moveElement element (x,y) = do
  let pos = [("left", show x ++ "px"), ("top", show y ++ "px")]
  void $ UI.pure element # set style (getDragableStyle ++ pos)


getDragableStyle :: [(String, String)]
getDragableStyle = [("height", "50px"), ("width", "50px"),
                    ("border-style", "solid"),
                    ("position", "absolute"),
                    ("background", "blue")]

-- Calculates the new position of an element based on the position of the mouse,
-- the elements current position and its dimensions
calculatePos :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
calculatePos mousePos oldPos dims =
  (fst mousePos + (fst oldPos - fst dims `div` 2), snd mousePos + (snd oldPos - snd dims `div` 2))
