module GlossRunner where

import Graphics.Gloss as GG

import Play.Gloss.Handlers
import Play.State

import Anim.Gloss.Handlers
import Anim.AnimationLib as ALib
import Anim.AST

bgColor :: GG.Color
bgColor = white

runGlossPlay :: PlayState -> IO ()
runGlossPlay state = play (InWindow "Scritch Output" (500, 500) (10, 10)) bgColor 60 state drawState eventHandler stepState

runGlossAnim :: [(Object, AnimationSeq)] -> IO ()
runGlossAnim objAn = animate (InWindow "Scritch Output" (500, 500) (10, 10)) bgColor $ pictures . myListAnimator objAn