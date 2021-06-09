module Play.Gloss.Handlers where

import Play.State 
import Play.Object
import Play.Event
import Play.Evaluator


import Graphics.Gloss as GG
import Graphics.Gloss.Interface.IO.Game
import Data.Map.Strict as M

-- This is where the special functions to running the state with Gloss reside.
-- To run with play with Gloss we need:
--   A State (PlayState)
--   Draw function :: State -> Picture
--   Input handler :: Event -> State -> State (Event is the Gloss' defn, not ours)
--   Step function to progress the world :: Float -> State -> State (This takes seconds passed for the float)

--This is the draw function, it pulls out the object map and passed it along, combining the result list with Pictures
drawState :: PlayState -> Picture
drawState state = Pictures $ drawObjects objMap
    where objMap =  objects state :: ObjMap

-- This takes the map and does a fancy foldr into a list of Picture
drawObjects :: ObjMap  -> [Picture]
drawObjects = M.foldr ((:) . objToPict) [Blank]

--This is how each picture is drawn for an object, visible is screened first, then shape 
objToPict :: Object -> Picture
objToPict obj = if not $ visible obj
                then Blank
                else case shape obj of
                    Play.Object.Circle -> Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (GG.Circle (Play.Object.size obj / 2))))
                    Play.Object.Rect x -> Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (GG.rectangleWire (Play.Object.size obj) (Play.Object.size obj * x))))

--This is how a Gloss event is parsed down and the events are run.
eventHandler :: Event -> PlayState -> PlayState 
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) state = state {objects = runEvents KbUp (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) state = state {objects = runEvents KbDown (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = state {objects = runEvents KbLeft (events state) (objects state) 0}
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) state = state {objects = runEvents KbRight (events state) (objects state) 0}
eventHandler (EventMotion (x,y) ) state = state {objects = secondObj}
    where firstObj = runEvents MouseX (events state) (objects state) (x+250) :: ObjMap
          secondObj = runEvents MouseY (events state) firstObj (y+250) :: ObjMap
eventHandler  _ state = state

--To constant update the state this stepping function is required.
--  Here any events Triggered by Always are run each time.
--  Then collisions are checked for
stepState :: Float -> PlayState -> PlayState 
stepState elapsed state = collisStep
    where 
        frames = elapsed * 60
        alwaysStep = state {objects = runEvents Always (events state) (objects state) frames}
        collisStep = runCollisions alwaysStep