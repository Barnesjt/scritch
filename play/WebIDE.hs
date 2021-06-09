module WebIDE where

import Control.Monad ( void )
import Control.Concurrent ( runInBoundThread )
import Control.Applicative
import Data.IORef ( modifyIORef, newIORef, readIORef )

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


--This runs the IDE, taking some function to run parsed input
--Currently it's a String, but it should be changed in the future
runIDE :: (String -> IO ()) -> IO ()
runIDE parseAndRun = startGUI defaultConfig $ setup parseAndRun parseAndRun

--This is where the UI is defined
setup :: (String -> IO()) -> (String -> IO()) -> Window -> UI ()
setup run1 run2 w = void $ do
    return w # set title "Scritch IDE"  --browser window title
    elRun   <- UI.button # set UI.text "Run"
    elInput   <- UI.textarea # set value "" # set UI.cols "80" # set UI.rows "10"
    elOutput <- UI.p # set UI.text "Try typing in a program" --Output msg
    elAnimOp <- UI.option # set UI.value "1" # set UI.text "Animate"
    elPlayOp <- UI.option # set UI.value "2" # set UI.text "Play"
    elModeSel <- UI.select # set children [elAnimOp, elPlayOp]

    let
        --Define how to draw the layout (attach as children in the DOM)
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout
            getBody w # set children [layout]

        --This defined the shape of the layout (row of buttons, hr, textarea)
        mkLayout :: UI Element
        mkLayout = column [row [element elModeSel, element elRun], UI.hr, element elInput, element elOutput]

        --Call but the Run onClick, gets the textarea's text, parses it
          -- and spawns the Gloss output (locally) to the "backend"
        runInput :: UI ()
        runInput = void $ do
            inputProg <- get value elInput
            mode <- get value elModeSel
            element elOutput # set UI.text "Output running. Close child window before running output again"
            liftIO $ runInBoundThread $ case mode of 
                                        "1" -> run1 inputProg
                                        "2" -> run2 inputProg
    on UI.click elRun $ const runInput

    --Finally we need to actually draw the layout with this final statement of the do block
    redoLayout
