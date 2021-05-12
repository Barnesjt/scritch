module Lib where

import Data.Fixed

data Transformation =
    Pivot Float
  | Move Float Float
  | Grow Float
  | Step Float
  | Wait
  | Combine [Transformation]
  deriving (Show, Eq)

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



type TimedTransformation = (Float, Transformation)

type AnimationSeq = [TimedTransformation]

rad :: Float -> Float
rad x = x * (pi / 180.0)


doTransform :: Transformation -> Object -> Object
doTransform Wait        obj = obj
doTransform (Pivot x)  obj = obj { dir = dir obj + x `mod'` 360 }
doTransform (Move x y)  obj = obj { posx = x, posy = y}
doTransform (Grow x)    obj = obj { size = size obj * x}
doTransform (Step x)    obj = obj { posx = posx obj + moveX * x , posy = posy obj + moveY * x}
                              where radDir = rad $ dir obj
                                    moveX = x * cos radDir
                                    moveY = x * sin radDir
doTransform (Combine []) obj     = obj
doTransform (Combine (x:xs)) obj = doTransform (Combine xs) $ doTransform x obj

stepBack :: Float -> Transformation
stepBack x = Combine [Pivot 180, Step x, Pivot 180]

intermediateObj :: Object -> Float -> Object -> Object
intermediateObj obj elapsed obj' 
                | elapsed > 1.0 = obj
                | otherwise = obj { 
                    posx = getMid (posx obj) (posx obj') elapsed,
                    posy = getMid (posy obj) (posy obj') elapsed,
                    posz = getMid (posz obj) (posz obj') elapsed,
                    size = getMid (size obj) (size obj') elapsed,
                    dir  = getMid (dir obj) (dir obj') elapsed
                  }

getMid :: Float -> Float -> Float -> Float
getMid start end ratio = (end - start) * ratio + start


doTimedTransform :: TimedTransformation -> Object -> Float -> Object
doTimedTransform (seconds, trans) obj elapsed
    = intermediateObj obj (elapsed / seconds) (doTransform trans obj)

doAnimation :: AnimationSeq -> Object -> Float -> Object
doAnimation (tt@(sec, trans) : xs) obj elapsed 
      | elapsed > sec = doAnimation xs (doTransform trans obj) (elapsed - sec)
      | otherwise     = doTimedTransform tt obj elapsed
doAnimation [] obj _ = obj

getAniLength :: AnimationSeq -> Float
getAniLength [] = 0
getAniLength ((s, _) : xs) = s + getAniLength xs

