{-# LANGUAGE TupleSections #-}
module PlayStateLib where

import qualified Data.Map.Strict as M

import Data.List

import qualified EventLib as Ev
import ObjectLib

data PlayState = Env
    { objects :: ObjMap
    , events :: [Ev.EventAction]
    }

--example Objects
obj1 :: Object
obj1 = Object {shape = Circle, visible = True, posx = 250, posy = 250, posz = 0, size = 20, dir = -180}

obj2 :: Object
obj2 = Object {shape = Rect 10, visible = True, posx = 50, posy = 250, posz = 0, size = 10, dir = 0}

obj3 :: Object
obj3 = Object {shape = Rect 10, visible = True, posx = 450, posy = 250, posz = 0, size = 10, dir = 0}

obj4 :: Object
obj4 = Object {shape = Rect 1, visible = True, posx = 250, posy = 750, posz = 0, size = 500, dir = 0}

obj5 :: Object
obj5 = Object {shape = Rect 1, visible = True, posx = 250, posy = -250, posz = 0, size = 500, dir = 0}

obj6 :: Object
obj6 = Object {shape = Rect 1, visible = True, posx = -250, posy = 250, posz = 0, size = 500, dir = 0}

obj7 :: Object
obj7 = Object {shape = Rect 1, visible = True, posx = 750, posy = 250, posz = 0, size = 500, dir = 0}

obja :: Object
obja = Object {shape = Circle, visible = True, posx = 350, posy = 250, posz = 0, size = 20, dir = -180}

objb :: Object
objb = Object {shape = Circle, visible = True, posx = 150, posy = 250, posz = 0, size = 20, dir = 0}





--example ObjectMap
pongObjs :: M.Map String Object
pongObjs = M.fromList [("ball", obj1), ("paddle1", obj2), ("paddle2", obj3), ("topbound", obj4), ("bottombound", obj5), ("leftbound", obj6), ("rightbound", obj7)]


testObjs :: ObjMap
testObjs = M.fromList[("b1", obja), ("b2", objb)]

-- Example using the Generalized Construction
stepBall :: Float -> Ev.EventAction
stepBall x = Ev.genEvent Ev.Always "ball" step (x *)

stepObj :: String -> Float -> Ev.EventAction
stepObj name x = Ev.genEvent Ev.Always name step (x *)

bounce :: String -> String -> Ev.EventAction
bounce name name' = Ev.genEvent (Ev.Collide name name') name pivot (const 55)

collideReset :: String -> String -> (Float, Float) -> Ev.EventAction
collideReset name name' (x, y) = Ev.genEvent (Ev.Collide name name') name (\_ obj -> moveY y (moveX x obj)) id

followY :: String -> Ev.EventAction
followY name = Ev.genEvent Ev.MouseY name moveY id

pongEvents :: [Ev.EventAction]
pongEvents =    [stepBall 5
                , bounce "ball" "paddle1"
                , bounce "ball" "paddle2"
                , bounce "ball" "topbound"
                , bounce "ball" "bottombound"
                , collideReset "ball" "leftbound" (250,250)
                , collideReset "ball" "rightbound" (250, 250)
                , followY "paddle1"
                , followY "paddle2"
                ]

testEvents :: [Ev.EventAction]
testEvents = [stepObj "b1" 1, stepObj "b2" 1, bounce "b1" "b2"]

testState :: PlayState
testState = Env {objects = testObjs, events = testEvents}

pongState :: PlayState
pongState = Env {objects = pongObjs, events = pongEvents}


runEvents :: Ev.EventTrigger -> [Ev.EventAction] -> ObjMap -> Float -> ObjMap
runEvents event [] base frames = base
runEvents event (e:es) base frames = runEvents event es (e event base frames) frames

runCollisions :: PlayState -> PlayState
runCollisions state = runCollisionsHelper objPairs state
    where objs = objects state
          evs = events state
          objPairs = pairs $ M.toList objs

runCollisionsHelper :: [((String, Object),(String,Object))] -> PlayState -> PlayState
runCollisionsHelper [] state = state
runCollisionsHelper (((k,o),(k',o')):ps) state = if testCollide o o' then runCollisionsHelper ps state{objects=newObjs} else runCollisionsHelper ps state
    where
        newObjs = runEvents (Ev.Collide k k') (events state) (objects state) 0
        newObjs2 = runEvents (Ev.Collide k k') (events state) newObjs 0


pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs