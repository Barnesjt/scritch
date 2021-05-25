{-# LANGUAGE GADTs #-}

module Lib where

import Data.Fixed

--Transformation is a minimal set of all possible Transformations.
-- This includes a combine type constructor for compound transformations.

-- Pivot is for rotating from the current angle
-- Move is an absolute move to new coordinates
-- Grow will change the object size by a factor (1.0 means it stays the same)
-- Step will move a distance in the direction it faces
-- Wait does nothing (remain static)
-- Combine sequences multiple transformations, eval'd left to right
data Expr a where


    -- Transformations
    -- We assume that functions which return null are moving objects,
    -- because object movement is the only side affect
    Pivot   :: Float -> Transformation
    Move    :: Float -> Float -> Transformation
    Grow    :: Float -> Transformation
    Step    :: Float -> Transformation
    Wait    :: Transformation
    Combine :: [Transformation] -> Transformation

type Transformation = Expr ()

-- Object is defined with record syntax
-- name is for internal referencing
-- display is meant to be parsed to the displayed object (such as a shape)
-- posx and posy are locations on the screen (0,0 is center for Gloss)
-- posz would be the depth in the scene to render the object (postive on "top")
-- size for the draw size, For a circle this is the radius
-- dir is the direction the object "faces", ie the direction it will step in. (East is 0 degrees)
data Object =
  Object
    { name :: String,
      disp :: String,
      posx :: Float,
      posy :: Float,
      posz :: Float,
      size :: Float,
      dir :: Float
    } deriving (Show)

-- This a a type synonym for a Transformation over time.
--   It will allow us to computed intermediate Objects (inbetween states)
type TimedTransformation = (Float, Transformation)

-- A animation is a list of TimedTransformations that occur in sequence starting with head
type AnimationSeq = [TimedTransformation]

--convience function for computing radians from degrees (which is what we store)
--  radians are needed for sin/cos for step
rad :: Float -> Float
rad x = x * (pi / 180.0)

-- This computes a new object from a transformation and an object
-- The return type is after the complete transformation
doTransform :: Transformation -> Object -> Object
doTransform Wait        obj = obj
doTransform (Pivot x)   obj = obj { dir = dir obj + x}
doTransform (Move x y)  obj = obj { posx = x, posy = y}
doTransform (Grow x)    obj = obj { size = size obj * x}
doTransform (Step x)    obj = obj { posx = posx obj + moveX * x , posy = posy obj + moveY * x}
                              where radDir = rad $ dir obj
                                    moveX = x * cos radDir
                                    moveY = x * sin radDir
doTransform (Combine []) obj     = obj
doTransform (Combine (x:xs)) obj = doTransform (Combine xs) $ doTransform x obj


-- Returns an object form a timed transformation with a starting object and the elapsed time.
--  Uses the tranformation time and elapsed time to computer intermediate objects
doTimedTransform :: TimedTransformation -> Object -> Float -> Object
doTimedTransform (seconds, trans) obj elapsed
    = case trans of
      Wait -> obj
      Pivot x  -> doTransform (Pivot (ratio x)) obj
      Move x y -> doTransform (Move (ratio diffX + x) (ratio diffY + y)) obj
        where diffX = posx obj - x -- interpolate from posx obj, posy obj
              diffY = posy obj - y
      Grow x -> doTransform (Grow (ratio x)) obj
      Step x -> doTransform (Step (ratio x)) obj
      Combine [] -> obj
      Combine (x:xs) -> doTimedTransform (seconds, x) (doTimedTransform (seconds, Combine xs) obj elapsed) elapsed
  where ratio x = (elapsed / seconds) * x
--    = intermediateObj obj (elapsed / seconds) (doTransform trans obj)



-- Returns an object from an animationSeq, using a starting Object and elapsed time.
--   Recurses on itself to find the current starting state (after last completed transform)
--    And then computes an intermediate object. If past the end of the animation, return final object
doAnimation :: AnimationSeq -> Object -> Float -> Object
doAnimation (tt@(sec, trans) : xs) obj elapsed
      | elapsed > sec = doAnimation xs (doTransform trans obj) (elapsed - sec)
      | otherwise     = doTimedTransform tt obj elapsed
doAnimation [] obj _ = obj

-- Convienence function to be used in app for repeating animations with modulus animation length
getAniLength :: AnimationSeq -> Float
getAniLength [] = 0
getAniLength ((s, _) : xs) = s + getAniLength xs
