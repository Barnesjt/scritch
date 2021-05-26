module WebIDE where

import Parser ( parseInput )
import AnimationLib ( Object, AnimationSeq )

import Control.Monad ( void )
import Control.Concurrent ( runInBoundThread )
import Control.Applicative
import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


--This runs the IDE, taking some function to run parsed input
--Currently it's a String, but it should be changed in the future
runIDE :: ([(Object, AnimationSeq)] -> IO ()) -> IO ()
runIDE glossRunner = startGUI defaultConfig $ setup glossRunner

--This is where the UI is defined
setup :: ([(Object, AnimationSeq)] -> IO()) -> Window -> UI ()
setup glossRunner w = void $ do
    return w # set title "Scritch IDE"  --browser window title
    elAdd    <- UI.button # set UI.text "Add Object" --Add and remove obj buttons
    elRemove <- UI.button # set UI.text "Remove Object"
    elRun   <- UI.button # set UI.text "Run"
    elOutput <- UI.p # set UI.text "Try typing in a program" --Output msg

    objects   <- liftIO $ newIORef []

    let
        addObject :: UI ()
        addObject = do
            elObject <- UI.textarea # set value ""
            liftIO $ modifyIORef objects ( ++ [elObject])

        removeObject :: UI ()
        removeObject = liftIO $ modifyIORef objects init

        --Define how to draw the layout (attach as children in the DOM)
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef objects)
            getBody w # set children [layout]

        --This defined the shape of the layout (row of buttons, hr, textarea)
        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $ [row [element elAdd, element elRemove, element elRun],UI.hr] ++ map element xs ++ [element elOutput]

        --Call but the Run onClick, gets the textarea's text, parses it
          -- and spawns the Gloss output (locally) to the "backend"
        runInput :: UI ()
        runInput = void $ do
            getObjList <- mapM (get value) =<< liftIO (readIORef objects)
            element elOutput # set UI.text "Output running. Close child window before running output again"
            liftIO $ runInBoundThread $ glossRunner (map parseInput getObjList)

    --Event handlers for the buttons
    on UI.click elAdd    $ \_ -> addObject    >> redoLayout
    on UI.click elRemove $ \_ -> removeObject >> redoLayout
    on UI.click elRun $ const runInput

    --Finally we need to actually draw the layout with this final statement of the do block
    addObject >> redoLayout
