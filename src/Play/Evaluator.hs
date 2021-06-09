{-# LANGUAGE TupleSections #-}
module Play.Evaluator where

import Play.State ( PlayState(events, objects) )
import Play.Event ( EventAction, EventTrigger(Collide) )
import Play.Object ( ObjMap, Object )
import Play.Collision ( testCollide )

import qualified Data.Map.Strict as M

-- The real meat of the event-driven system. Takes a trigger, and runs it against all the events
--      folding into the result, speaking of which I'm sure this could be a fold, oh well.
runEvents :: EventTrigger -> [EventAction] -> ObjMap -> Float -> ObjMap
runEvents event [] base frames = base
runEvents event (e:es) base frames = runEvents event es (e event base frames) frames

-- Master function to test for collisions. Gets all unique pairs of objects.
--   Checks each one for collisions, and passes it into the events if so.
runCollisions :: PlayState -> PlayState
runCollisions state = runCollisionsHelper objPairs state
    where objs = objects state
          evs = events state
          objPairs = pairs $ M.toList objs

-- This takes the unique pair list, tests the collision, and runs events both ways (once for each object first)
runCollisionsHelper :: [((String, Object),(String,Object))] -> PlayState -> PlayState
runCollisionsHelper [] state = state
runCollisionsHelper (((k,o),(k',o')):ps) state = if testCollide o o' then runCollisionsHelper ps state{objects=newObjs} else runCollisionsHelper ps state
    where
        newObjs = runEvents (Collide k k') (events state) (objects state) 0
        newObjs2 = runEvents (Collide k k') (events state) newObjs 0

-- A function to pull out all unique pairs from a list
pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
