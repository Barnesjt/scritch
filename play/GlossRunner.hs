module GlossRunner where

import Graphics.Gloss as GG
    ( white, play, Display(InWindow), Color )

import Play.Gloss.Handlers ( drawState, eventHandler, stepState )
import Play.State

bgColor :: GG.Color
bgColor = white


run :: PlayState -> IO ()
run = play (InWindow "Scritch Output" (500, 500) (10, 10)) bgColor 60 pongGame drawState eventHandler stepState
