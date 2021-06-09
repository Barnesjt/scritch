module TestDefns where

import Play.State
import Play.Object
import Play.Event
import Play.EventCons

import Graphics.Gloss.Interface.IO.Game ( Key(..) )

import Data.Map.Strict as M (fromList)

-- Here are all the definitions needed for the Pong example
obj1, obj2, obj3, obj4, obj5, obj6, obj7 :: Object
obj1 = Object {shape = Circle, visible = True, posx = 250, posy = 250, posz = 0, size = 20, dir = -180}
obj2 = Object {shape = Rect 10, visible = True, posx = 50, posy = 250, posz = 0, size = 10, dir = 0}
obj3 = Object {shape = Rect 10, visible = True, posx = 450, posy = 250, posz = 0, size = 10, dir = 0}
obj4 = Object {shape = Rect 1, visible = True, posx = 250, posy = 750, posz = 0, size = 500, dir = 0}
obj5 = Object {shape = Rect 1, visible = True, posx = 250, posy = -250, posz = 0, size = 500, dir = 0}
obj6 = Object {shape = Rect 1, visible = True, posx = -250, posy = 250, posz = 0, size = 500, dir = 0}
obj7 = Object {shape = Rect 1, visible = True, posx = 750, posy = 250, posz = 0, size = 500, dir = 0}

pongObjs :: ObjMap
pongObjs = M.fromList [ ("ball", obj1),
                        ("paddle1", obj2),
                        ("paddle2", obj3),
                        ("topbound", obj4),
                        ("bottombound", obj5),
                        ("leftbound", obj6),
                        ("rightbound", obj7) ]

pongEvents :: [EventAction]
pongEvents =    [stepBall 5
                , bounce "ball" "paddle1"
                , bounce "ball" "paddle2"
                , bounce "ball" "topbound"
                , bounce "ball" "bottombound"
                , collideReset "ball" "leftbound" (250,250)
                , collideReset "ball" "rightbound" (250, 250)
                , followY "paddle1"
                , genEvent (GlossKey (Char 'w')) "paddle2" stepY (const 10)
                , genEvent (GlossKey (Char 's')) "paddle2" stepY (const (-10))
                ]

pongGame :: PlayState
pongGame = Env {objects = pongObjs, events = pongEvents}
