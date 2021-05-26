module WebIDE where

import Parser ( parseInput )
import AnimationLib ( Object, AnimationSeq )

import Control.Monad ( void )
import Control.Concurrent ( runInBoundThread )

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


--This runs the IDE, taking some function to run parsed input
--Currently it's a String, but it should be changed in the future
runIDE :: ((Object, AnimationSeq) -> IO ()) -> IO ()
runIDE glossRunner = startGUI defaultConfig $ setup glossRunner

--This is where the UI is defined
setup :: ((Object, AnimationSeq) -> IO()) -> Window -> UI ()
setup glossRunner w = void $ do
    return w # set title "Scritch IDE"  --browser window title
    elProg  <- UI.textarea              --text area (TODO: should be bigger by default)
    elClear <- UI.button # set UI.text "Clear" --Two buttons
    elRun   <- UI.button # set UI.text "Run"
    elOutput <- UI.p # set UI.text "Try typing in a program"

    let
        --Define how to draw the layout (attach as children in the DOM)
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout
            getBody w # set children [layout]

        --This defined the shape of the layout (row of buttons, hr, textarea)
        mkLayout :: UI Element
        mkLayout = column [row [element elClear, element elRun],UI.hr, element elProg, element elOutput]

        --Called by the Clear onClick, wipes the text area
        clearInput :: UI ()
        clearInput = void $ element elProg # set UI.value ""

        --Call but the Run onClick, gets the textarea's text, parses it
          -- and spawns the Gloss output (locally) to the "backend"
        runInput :: UI ()
        runInput = void $ do
            inputProg <- get value elProg
            case parseInput inputProg of
                Nothing -> void $ element elOutput # set UI.text "Parsing error. Unable to spawn output."
                Just i -> do
                    element elOutput # set UI.text "Output running. Close child window before running output again"
                    liftIO $ runInBoundThread $ glossRunner i

    --Event handlers for the buttons
    on UI.click elClear $ const clearInput
    on UI.click elRun $ const runInput

    --Finally we need to actually draw the layout with this final statement of the do block
    redoLayout
