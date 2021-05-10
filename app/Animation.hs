module Animation where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import AST
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

win :: Display
win = InWindow "My Awesome Window" (600, 600) (100 , 100)

fps :: Int
fps = 30

newtype Model = M (Map String (Float, Float)) -- models the position of all objects
    deriving (Eq, Show)

data State = S [Stmt] Model Float -- state consists of the program, model, and time since last tick
    deriving (Eq, Show)

render :: State -> Picture
render (S _ (M s) _) = pictures $ map (\(x, y) -> translate x y (Circle 20)) $ Map.elems s

inputHandler :: Event -> State -> State
inputHandler _ = id

update :: Float -> State -> State
update dt (S []     m tick) = S [] m (tick + dt)
update dt (S (c:cs) m tick) = let m' = stmt m c
                              in  if tick > 1 -- update every second
                                  then updateProg c $ update dt (S cs m' tick)
                                  else S (c:cs) m (tick + dt)


translateObj :: String -> Float -> Float -> Map String (Float, Float) -> Map String (Float, Float) -- update position inside map
translateObj o dx dy m = Map.adjust (\(x, y) -> (x + dx, y + dy)) o m

stmt :: Model -> Stmt -> Model -- stmt applies the closest function to an object, ignoring others
stmt (M m) (Base (Obj o)) = M m
stmt (M m) (App (Move dx dy) (Base (Obj o))) = M (translateObj o dx dy m)
stmt (M m) (App _ s) = stmt (M m) s
-- stmt (M m) (App (Move dx dy) s)
--stmt (M m) ()

removeFun :: Stmt -> Stmt -- strips the closest function to an object, for use in sequential animating
removeFun (Base b)         = Base b
removeFun (App _ (Base b)) = Base b
removeFun (App f s)        = App f (removeFun s)

updateProg :: Stmt -> State -> State -- maps removeFun
updateProg s (S cmds m dt) = S ((removeFun s) : cmds) m 0

getObj :: Stmt -> String -- gets the object from a function application
getObj (Base (Obj s)) = s
getObj (App _ s) = getObj s

getState :: [Stmt] -> State -- converts a program to a state
getState ss = S ss (M (Map.fromList (map (\s -> (getObj s, (0, 0))) ss))) 0

animateProg :: [Stmt] -> IO ()
animateProg p = play win white fps (getState p) render inputHandler update

testProg :: [Stmt]
testProg = [App (Move 100 100) (App (Move (-100) (-100)) (obj "1")), App (Move 200 200) (App (Move (100) (-100)) (obj "2"))]

main :: IO ()
main = animateProg testProg

-- code is super messy right now, but it's a start
