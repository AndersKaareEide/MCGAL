import           Control.Monad


import           Checker
import           Examples
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Text.Read as Text
import           Data.Maybe (fromJust)

import Paths

main :: IO ()
main = do
  path <- Paths.getStaticDir
  startGUI defaultConfig {

    jsCustomHTML = Just "index.html"
  , jsStatic = Just path
    } setup

setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Dix"
  getBody window #+ [
      UI.h3 #+ [string checkString],
      UI.h3 #+ [string modelString],
      UI.h3 # set UI.id_ "result" #+ [string $ show result],
      UI.input # set UI.id_ "input",
      testButton window
    ] where
    checkString = "Checking: M," ++ show state ++ " |= " ++ show formula
    modelString = "M: " ++ show model
    result = check model state formula
    model = exampleModel2
    formula = Prop "p"
    state = 2

testButton :: Window -> UI Element
testButton window = do
  button <- UI.button #+ [string "A button"] # set style [("color","#FF0000")]
  on UI.click button $ \_ -> liftIO $ printValue window
  return button

printValue :: Window -> IO ()
printValue window = do
  let ioInputElement = fromJust <$> UI.getElementById window "input" :: UI Element
  let ioCheckHeader = fromJust <$> UI.getElementById window "result" :: UI Element
  inputElement <- runUI window ioInputElement
  inputText <- runUI window (get value inputElement)
  let result = runChecker inputText
  runUI window (ioCheckHeader # set text result)
  return ()


-- #. = Setting the CSS class on element creation

runChecker :: String -> String
runChecker inputString =
  let readResult = Text.readEither inputString :: Either String Int
  in case readResult of
    Right state ->  show $ check exampleModel2 state (Prop "p")
    Left _ -> "Failed to parse string"