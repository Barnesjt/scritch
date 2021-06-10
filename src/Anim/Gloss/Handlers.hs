module Anim.Gloss.Handlers where

import Anim.AnimationLib as ALib
import Anim.AST

import Graphics.Gloss as GG
import Data.Fixed ( mod' )
import Data.List ( find )


myListAnimator :: [(Object, AnimationSeq)] -> Float -> [Picture]
myListAnimator [] _       = [blank]
myListAnimator ((o,a):as) x = case find (\(obj, _) -> disp obj == "Error") ((o,a):as) of
    Just (o', _) -> [myAnimation a o' x]
    Nothing      -> myAnimation a o x : myListAnimator as x

-- Creates the animation function as Gloss expects it (Float -> Picture)
-- getAniLength used to loop animation
-- obj and anim used to generate the "intermediate" object from second elapsed
myAnimation :: AnimationSeq -> Object -> Float -> Picture
myAnimation anim obj x = case disp obj of
    "Error" -> Translate (-250) 0 (Scale 0.1 0.1 (Text (name obj)))
    _ -> objToPic $ doAnimation anim obj elapsed
    where elapsed = x `mod'` getAniLength anim

-- Function to turn any object into a picture. Order matters (translate last!)
-- This will need to be expanded to parse out different display values for objects
objToPic :: Object -> Picture
objToPic obj = Translate (-250) (-250) (Translate (posx obj) (posy obj) (Rotate (dir obj) (Circle (size obj))))

