{-# LANGUAGE GADTs #-}

module Lib where

import Data.Fixed
import Prelude hiding (LT, GT, EQ)

--Transformation is a minimal set of all possible Transformations.
-- This includes a combine type constructor for compound transformations.

-- Pivot is for rotating from the current angle
-- Move is an absolute move to new coordinates
-- Grow will change the object size by a factor (1.0 means it stays the same)
-- Step will move a distance in the direction it faces
-- Wait does nothing (remain static)
-- Combine sequences multiple transformations, eval'd left to right
data Expr a where
    -- Literals
    Lit     :: a -> Expr a

    -- binary operators
    Bin     :: Function (a -> b -> c) -> Expr a -> Expr b -> Expr c

    -- conditional
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

    -- retrieve info from an Object (should we represent this using Function?)
    Get     :: Object -> (ObjectField a) -> Expr a

    -- Transformations
    Pivot   :: Expr Float -> Transformation
    Move    :: Expr Float -> Expr Float -> Transformation
    Grow    :: Expr Float -> Transformation
    Step    :: Expr Float -> Transformation
    Wait    :: Transformation
    Combine :: [Transformation] -> Transformation

---- We assume that expressions which evaluate to null are moving objects,
-- because object movement is the only side effect. The type synonym reflects this.
type Transformation = Expr ()

-- type for all functions
data Function a where
    Add :: Num a => Function (a -> a -> a)
    Mul :: Num a => Function (a -> a -> a)
    Sub :: Num a => Function (a -> a -> a)

    And :: Function (Bool -> Bool -> Bool)
    Or  :: Function (Bool -> Bool -> Bool)

    LT  :: Ord a => Function (a -> a -> Bool)
    GT  :: Ord a => Function (a -> a -> Bool)
    EQ  :: Eq  a => Function (a -> a -> Bool)


-- data type of Object field references, for use with Get
data ObjectField a where
    Name :: ObjectField String
    Disp :: ObjectField String
    PosX :: ObjectField Float
    PosY :: ObjectField Float
    PosZ :: ObjectField Float
    Size :: ObjectField Float
    Dir  :: ObjectField Float

-- evaluate an abstract expression -- currently don't know what to do with Tranformations
-- maybe we just won't deal with them here, because they are handles by the doTransform functions
eval :: Expr a -> a
eval (Lit a)     = a
eval (Bin f l r) = (op f) (eval l) (eval r)
    where
        op :: Function (a -> b -> c) -> (a -> b -> c)
        op Add = (+)
        op Mul = (*)
        op Sub = (-)

        op And = (&&)
        op Or  = (||)

        op LT  = (<)
        op GT  = (>)
        op EQ  = (==)
eval (If c t e)  = if eval c then eval t else eval e
eval (Get o f) = get f o where
    get :: ObjectField a -> Object -> a
    get Name = name
    get Disp = disp
    get PosX = posx
    get PosY = posy
    get PosZ = posz
    get Size = size
    get Dir  = dir


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
doTransform (Pivot x)   obj = obj { dir = dir obj + eval x}
doTransform (Move x y)  obj = obj { posx = eval x, posy = eval y}
doTransform (Grow x)    obj = obj { size = size obj * eval x}
doTransform (Step x)    obj = obj { posx = posx obj + moveX * eval x , posy = posy obj + moveY * eval x}
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
