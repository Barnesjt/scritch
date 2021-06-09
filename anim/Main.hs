module Main where

import WebIDE ( runIDE )
import GlossRunner ( runGloss )
import Web.Browser (openBrowser)


-- Open a browser to the endpoint, then run the IDE with a reference (argument) to run Gloss
main :: IO ()
main = do 
    openBrowser "http://localhost:8023"
    runIDE runGloss
