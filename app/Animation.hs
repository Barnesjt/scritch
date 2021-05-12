module Animation where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import AST
import Control.Monad
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

win :: Display
win = InWindow "My Awesome Window" (600, 600) (100 , 100)

fps :: Int
fps = 30

type Model = Map String Basic -- maps variables to data

data State = S [Stmt] Model Float -- state consists of the program, model, and time since last tick
    deriving (Eq, Show)

render :: State -> Picture
render s = pictures $ map (\(x, y) -> translate x y (Circle 20)) (getObjectPositions s)

inputHandler :: Event -> State -> State
inputHandler _ = id

update :: Float -> State -> State
update dt (S []     m tick) = S [] m (tick + dt)
update dt (S (c:cs) m tick) = let m' = stmt m c
                              in  if   tick > 1 -- update every second
                                  then updateProg c $ update dt (S cs m' tick)
                                  else S (c:cs) m (tick + dt)


translateObj :: String -> Float -> Float -> Model -> Model -- update position inside map
translateObj v dist dir m = case Map.lookup v m of
    Just (Obj x y) -> Map.insert v (Obj (x + dist * (cos dir)) (y + dist * (sin dir))) m
    _              -> m

stmt :: Model -> Stmt -> Model -- stmt applies the closest function to an object, ignoring others
stmt m (Base _ (Obj _ _))                       = m
stmt m (App (Move dist dir) (Base v (Obj _ _))) = translateObj v dist dir m
stmt m (App _ s)                                = stmt m s

removeFun :: Stmt -> Stmt -- strips the closest function to an object, for use in sequential animating
removeFun (Base v b)         = Base v b
removeFun (App _ (Base v b)) = Base v b
removeFun (App f s)         = App f (removeFun s)

updateProg :: Stmt -> State -> State -- maps removeFun
updateProg s (S cmds m dt) = S ((removeFun s) : cmds) m 0

getBase :: Stmt -> (String, Basic) -- gets the name of the base from a function application
getBase (Base v o) = (v, o)
getBase (App _ s) = getBase s

getObjectPositions :: State -> [(Float, Float)]
getObjectPositions (S _ m _) = let basics = Map.elems m
                                   p (Obj x y) = Just (x, y)
                                   p _ = Nothing
                                   objs = map p basics
                               in  catMaybes objs

getState :: [Stmt] -> State -- converts a program to a state
getState ss = S ss (Map.fromList (map getBase ss)) 0

animateProg :: [Stmt] -> IO ()
animateProg p = play win white fps (getState p) render inputHandler update

testProg :: [Stmt]
testProg = [App (Move 100 (pi/4)) (App (Move (100) (pi/2)) (obj "1" 0 0)), App (Move 200 (pi/4)) (App (Move (100) (-pi/2)) (obj "2" 50 50))]

-- code is super messy right now, but it's a start

-- "extending" with some syntactic sugar for shapes
square :: Float -> String -> Float -> Float -> Stmt
square l s x y = App (Move l 0)
                    (App (Move l (pi/2))
                        (App (Move l pi)
                            (App (Move l (3*pi/2))
                                (obj s x y))))

squareProg :: [Stmt]
squareProg = [square 100 "1" 100 100]

main :: IO ()
main = animateProg squareProg
