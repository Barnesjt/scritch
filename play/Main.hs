module Main where

import GlossRunner ( run )
import Web.Browser (openBrowser)

import Play.State
import TestDefns

main :: IO ()
main = GlossRunner.run




tempParser :: String -> PlayState
tempParser _ = pongGame 