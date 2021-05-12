module Main where

import Graphics.Gloss
import Lib as My
import Test as TestSt

import Data.Fixed

main :: IO ()
--main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
main = animate (InWindow "Animation Time" (500, 500) (10, 10)) white $ myAnimation TestSt.as1 TestSt.testObj

myAnimation :: AnimationSeq -> Object -> Float -> Picture
myAnimation anim obj x = objToPic $ doAnimation anim obj elapsed
    where elapsed = x `mod'` getAniLength anim

objToPic :: Object -> Picture
objToPic obj = Translate (posx obj) (posy obj) (Rotate (dir obj) (Circle (size obj)))

