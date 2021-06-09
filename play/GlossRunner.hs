module GlossRunner where

import Graphics.Gloss as GG
    ( white, play, Display(InWindow), Color )

import TestDefns ( pongGame)
import Play.Gloss.Handlers ( drawState, eventHandler, stepState )

bgColor :: GG.Color
bgColor = white


run :: IO ()
run = play (InWindow "Scritch Output" (500, 500) (10, 10)) bgColor 60 pongGame drawState eventHandler stepState
