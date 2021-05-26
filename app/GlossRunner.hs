module GlossRunner where

import Graphics.Gloss as GG
import Data.Fixed ( mod' )

import AnimationLib as ALib
import Test as TestSt

--This file is mostly up in the air right now
  --The code here is to demonstrate the project, but not intended to be final

--Color for the window background
bgColor :: GG.Color
bgColor = white

--Currently this just takes a string to put into the title
  --Should be modified to take the whatever type the parsed input would be
runGloss :: (Object, AnimationSeq) -> IO ()
runGloss (obj, ani) = animate (InWindow "Window" (500, 500) (10, 10)) bgColor $ myAnimation ani obj

-- Creates the animation function as Gloss expects it (Float -> Picture)
-- getAniLength used to loop animation
-- obj and anim used to generate the "intermediate" object from second elapsed
myAnimation :: AnimationSeq -> Object -> Float -> Picture
myAnimation anim obj x = objToPic $ doAnimation anim obj elapsed
    where elapsed = x `mod'` getAniLength anim

-- Function to turn any object into a picture. Order matters (translate last!)
-- This will need to be expanded to parse out different display values for objects
objToPic :: Object -> Picture
objToPic obj = Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (Circle (size obj))))
