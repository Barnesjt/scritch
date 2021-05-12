module Main where

import Graphics.Gloss
import Lib as My
import Test as TestSt

import Data.Fixed

--Animate Window BG (Float -> Picture)
main :: IO ()
--main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
main = animate (InWindow "Animation Time" (500, 500) (10, 10)) white $ myAnimation TestSt.as3 TestSt.testObj


-- Creates the animation function as Gloss expects it (Float -> Picture)
-- getAniLength used to loop animation
-- obj and anim used to generate the "intermediate" object from second elapsed
myAnimation :: AnimationSeq -> Object -> Float -> Picture
myAnimation anim obj x = objToPic $ doAnimation anim obj elapsed
    where elapsed = x `mod'` getAniLength anim


-- Function to turn any object into a picture. Order matters (translate last!)
-- This will need to be expanded to parse out different display values for objects
objToPic :: Object -> Picture
objToPic obj = Translate (posx obj) (posy obj) (Rotate (dir obj) (Circle (size obj)))

