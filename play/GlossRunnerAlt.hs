module GlossRunnerAlt where

import Graphics.Gloss as GG
import Graphics.Gloss.Interface.IO.Game

import PlayStateLib
import ObjectLib

import EventLib

import Data.Map.Strict as M

bgColor :: GG.Color
bgColor = white

run :: IO ()
run = play (InWindow "Scritch Output" (500, 500) (10, 10)) bgColor 60 pongState drawState eventHandler stepState


drawState :: PlayState -> Picture
drawState state = Pictures $ drawObjects objMap
    where objMap =  objects state :: ObjMap

drawObjects :: ObjMap  -> [Picture]
drawObjects = M.foldr ((:) . objToPict) [Blank]

objToPict :: Object -> Picture
objToPict obj = if not $ visible obj
                then Blank
                else case shape obj of
                    ObjectLib.Circle -> Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (GG.Circle (ObjectLib.size obj / 2))))
                    ObjectLib.Rect x -> Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (GG.rectangleWire (ObjectLib.size obj) (ObjectLib.size obj * x))))

eventHandler :: Event -> PlayState -> PlayState 
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) state = state {objects = runEvents KbUp (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) state = state {objects = runEvents KbDown (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = state {objects = runEvents KbLeft (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) state = state {objects = runEvents KbRight (events state) (objects state) 0}
eventHandler (EventMotion (x,y) ) state = state {objects = secondObj}
    where firstObj = runEvents MouseX (events state) (objects state) (x+250) :: ObjMap
          secondObj = runEvents MouseY (events state) firstObj (y+250) :: ObjMap
eventHandler  _ state = state

stepState :: Float -> PlayState -> PlayState 
stepState elapsed state = collisStep
    where 
        frames = elapsed * 60
        alwaysStep = state {objects = runEvents Always (events state) (objects state) frames}
        collisStep = runCollisions alwaysStep