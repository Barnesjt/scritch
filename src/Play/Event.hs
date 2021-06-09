module Play.Event where

import Play.Object ( ObjMap, Object )
import Data.Map.Strict as M ( lookup, insert )

data EventTrigger
    = Always
    | KbUp
    | KbDown
    | KbLeft
    | KbRight
    | MouseX
    | MouseY
    | Collide String String
    deriving(Eq, Show)

type EventAction = EventTrigger -> ObjMap -> Float -> ObjMap

-- Generalized Constructor
genEvent :: EventTrigger -> String -> (Float -> Object -> Object) -> (Float -> Float) -> EventAction
genEvent event name action modX ev base  x =
    if ev == event then (case M.lookup name base of
                             Nothing -> base
                             Just target -> M.insert name (action (modX x) target) base) else base
