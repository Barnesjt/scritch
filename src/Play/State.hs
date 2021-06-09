module Play.State where

import Play.Event ( EventAction )
import Play.Object ( ObjMap )

data PlayState = Env
    { objects :: ObjMap
    , events :: [EventAction]
    }
