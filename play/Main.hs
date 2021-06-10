module Main where

import WebIDE ( runIDE )
import GlossRunner
import Anim.Parser as AP
import Play.Parser as PP

import Web.Browser (openBrowser)
main :: IO ()
main = do 
    openBrowser "http://localhost:8023"
    runIDE (GlossRunner.runGlossAnim . map AP.parseInput . lines) (GlossRunner.runGlossPlay . PP.tempParser)