{-# LANGUAGE GADTs #-}

module AnimationLib where

import Prelude hiding (LT, GT, EQ)
import AST
import Eval

-- This a a type synonym for a Transformation over time.
--   It will allow us to computed intermediate Objects (inbetween states)
type TimedTransformation = (Float, Transformation)

-- A animation is a list of TimedTransformations that occur in sequence starting with head
type AnimationSeq = [TimedTransformation]

--convenience function for computing radians from degrees (which is what we store)
--  radians are needed for sin/cos for step
rad :: Float -> Float
rad x = x * (pi / 180.0)

-- This computes a new object from a transformation and an object
-- The return type is after the complete transformation
doTransform :: Transformation -> Object -> Object
doTransform Wait        obj = obj
doTransform (Pivot x)   obj = obj { dir = dir obj + eval x}
doTransform (Move x y)  obj = obj { posx = eval x, posy = eval y}
doTransform (Grow x)    obj = obj { size = size obj * eval x}
doTransform (Step x)    obj = obj { posx = posx obj + moveX, posy = posy obj + moveY}
                              where radDir = rad $ dir obj
                                    moveX = eval x * cos radDir
                                    moveY = eval x * sin radDir
doTransform (Combine []) obj     = obj
doTransform (Combine (x:xs)) obj = doTransform (Combine xs) $ doTransform x obj


-- Returns an object form a timed transformation with a starting object and the elapsed time.
--  Uses the tranformation time and elapsed time to compute intermediate objects
doTimedTransform :: TimedTransformation -> Object -> Float -> Object
doTimedTransform (seconds, trans) obj elapsed
    = case trans of
      Wait -> obj
      Pivot x  -> doTransform (Pivot $ Lit (ratio $ eval x)) obj
      Move x y -> doTransform (Move (Lit (ratio diffX + eval x)) (Lit (ratio diffY + eval y))) obj
        where diffX = posx obj - eval x -- interpolate from posx obj, posy obj
              diffY = posy obj - eval y
      Grow x -> doTransform (Grow $ Lit (ratio $ eval x)) obj
      Step x -> doTransform (Step $ Lit (ratio $ eval x)) obj
      Combine [] -> obj
      Combine (x:xs) -> doTimedTransform (seconds, x) (doTimedTransform (seconds, Combine xs) obj elapsed) elapsed
  where ratio x = elapsed / seconds * x


-- Returns an object from an animationSeq, using a starting Object and elapsed time.
--   Recurses on itself to find the current starting state (after last completed transform)
--    And then computes an intermediate object. If past the end of the animation, return final object
doAnimation :: AnimationSeq -> Object -> Float -> Object
doAnimation (tt@(sec, trans) : xs) obj elapsed
      | elapsed > sec = doAnimation xs (doTransform trans obj) (elapsed - sec)
      | otherwise     = doTimedTransform tt obj elapsed
doAnimation [] obj _ = obj

-- Convenience function to be used in app for repeating animations with modulus animation length
getAniLength :: AnimationSeq -> Float
getAniLength [] = 0
getAniLength ((s, _) : xs) = s + getAniLength xs
