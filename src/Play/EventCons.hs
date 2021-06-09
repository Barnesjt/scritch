module Play.EventCons where

import Play.Event
import Play.Object

stepBall :: Float -> EventAction
stepBall x = genEvent Always "ball" step (x *)

stepObj :: String -> Float -> EventAction
stepObj name x = genEvent Always name step (x *)

bounce :: String -> String -> EventAction
bounce name name' = genEvent (Collide name name') name pivot (const 55)

collideReset :: String -> String -> (Float, Float) -> EventAction
collideReset name name' (x, y) = genEvent (Collide name name') name (\_ obj -> moveY y (moveX x obj)) id

followY :: String -> EventAction
followY name = genEvent MouseY name moveY id