module EventLib where

import ObjectLib
import Data.Map.Strict as M ( lookup, insert )

data EventTrigger
    = Always
    | KbUp
    | KbDown
    | KbLeft
    | KbRight
    | MouseX
    | MouseY
    | Collide String String
    deriving(Eq, Show)

type EventAction = EventTrigger -> ObjMap -> Float -> ObjMap


-- Examples
stepBall :: EventTrigger -> ObjMap -> Float -> ObjMap
stepBall Always obM  x = case ballObj of
                        Nothing -> obM
                        Just ball -> M.insert "ball" (step x ball) obM
    where ballObj = M.lookup "ball" obM
stepBall _ obM  _ = obM

followMousePaddle1 :: EventTrigger -> ObjMap -> Float -> ObjMap
followMousePaddle1 MouseX obM x = case paddleObj of
                        Nothing -> obM
                        Just paddle -> M.insert "paddle1" (paddle {posx = x}) obM
    where paddleObj = M.lookup "paddle1" obM
followMousePaddle1 _ obM _ = obM


followMousePaddle2 :: EventTrigger -> ObjMap -> Float -> ObjMap
followMousePaddle2 MouseX obM x = case paddleObj of
                        Nothing -> obM
                        Just paddle -> M.insert "paddle2" (paddle {posx = x}) obM
    where paddleObj = M.lookup "paddle2" obM
followMousePaddle2 _ obM _ = obM

-- Generalized Construction
genEvent :: EventTrigger -> String -> (Float -> Object -> Object) -> (Float -> Float) -> EventAction
genEvent event name action modX ev base  x =
    if ev == event then (case M.lookup name base of
                             Nothing -> base
                             Just target -> M.insert name (action (modX x) target) base) else base

