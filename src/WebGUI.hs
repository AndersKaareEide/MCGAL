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
        -- active elements
        elRemove <- UI.button # set UI.text "Clear"
        elResult <- UI.span
        elCanvas <- UI.div # set style [("height", "500px"), ("width", "100%"),
                                        ("border-style", "solid")]

        elParent <- UI.div #  set style [("height", "100px"), ("width", "100px"),
                                        ("border-style", "solid")]
        elChild <- UI.button # set text "Child"

        UI.pure elParent #+ [UI.pure elChild]

        eventsRef <- liftIO $ newIORef [] :: UI (IORef [(String, IO ())])
        inputs   <- liftIO $ newIORef []


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
                displayTotal

            mkLayout :: [Element] -> UI Element
            mkLayout xs = column $
                [row [element elRemove]
                ,UI.hr]
                ++ map element xs ++
                [UI.hr
                ,row [UI.span # set text "Sum: ", element elResult]
                ,UI.pure elParent
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

        -- Try stopping event propagation

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

        -- Maybe store event as an ioRef and check ioRefs to see whether to
        -- dispatch a second event or not

        redoLayout
        -- TODO change (50,50)

handleEvents :: IORef [(String, IO ())] -> IO ()
handleEvents ref = do
  list <- readIORef ref
  snd . head $ list
