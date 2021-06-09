module Main where

import GlossRunner ( runGloss )
import Web.Browser (openBrowser)
import WebIDE ( runIDE )

import Play.State
import TestDefns

main :: IO ()
main = do 
    openBrowser "http://localhost:8023"
    runIDE $ GlossRunner.runGloss . tempParser




tempParser :: String -> PlayState
tempParser _ = pongGame 