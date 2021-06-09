module ObjectLib where

import Data.Map.Strict as M ( Map, fromList )

-- Object is defined with record syntax
-- name is for internal referencing
-- display is meant to be parsed to the displayed object (such as a shape)
-- posx and posy are locations on the screen (0,0 is center for Gloss)
-- posz would be the depth in the scene to render the object (postive on "top")
-- size for the draw size, For a circle this is the radius
-- dir is the direction the object "faces", ie the direction it will step in. (East is 0 degrees)
data Object =
  Object {
      shape :: Shape,
      visible :: Bool,
      posx :: Float,
      posy :: Float,
      posz :: Float,
      size :: Float,
      dir :: Float
    } deriving (Show)

type ObjMap = M.Map String Object

data Shape
    = Circle
    | Rect Float -- ratio hieght:width example 1 is square, 2 is twice as tall, .2 is twice as wide. For size 10: ratio 2 is 20 tall, 10 wide and ratio .5 is 5 tall, 10 wide
    deriving (Show)

data Boundry
    = Round Point Float               --centerX, centerY, Radius
    | Rectangle Point Point    --minX, minY, maxX, maxY

type Point = (Float, Float)

testCollide :: Object -> Object -> Bool
testCollide o o' = collideBoundry (getBoundry o) (getBoundry o')

getBoundry :: Object -> Boundry
getBoundry o@(Object {shape=Circle}) = Round (posx o, posy o) ((size o) / 2)
getBoundry o@(Object {shape=Rect ratio}) = Rectangle (x1, y1) (x2, y2)
    where
        halfwidth = (size o) / 2
        halfheight = (ratio * (size o)) / 2
        x1 = (posx o) - halfwidth
        x2 = (posx o) + halfwidth
        y1 = (posy o) - halfheight
        y2 = (posy o) + halfheight

collideBoundry :: Boundry -> Boundry -> Bool
collideBoundry  (Round (x,y) r)
                (Round (x',y') r')
                = distance (x,y) (x',y') < max r r'
collideBoundry  (Rectangle (minx,miny) (maxx,maxy))
                (Rectangle (minx',miny') (maxx',maxy'))
                = not (maxy >= minx' || maxx' >= minx || maxy >= miny' || maxy' >= miny)
collideBoundry  (Round (x,y) r)
                (Rectangle (minx,miny) (maxx,maxy))
                = dX < 2*r
                where nearest cx x' x'' = max x' (min cx x'')
                      nearX = nearest x minx maxx
                      nearY = nearest y miny maxy
                      dX = distance (nearX, nearY) (x, y)
collideBoundry b b' = collideBoundry b' b

--convenience function for computing radians from degrees (which is what we store)
--  radians are needed for sin/cos for step
rad :: Float -> Float
rad x = x * (pi / 180.0)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x, y) (x', y') = sqrt ((x' - x)**2 + (y' - y)**2)

step :: Float -> Object -> Object
step x obj = obj { posx = posx obj + moveX, posy = posy obj + moveY}
                              where radDir = rad $ dir obj
                                    moveX = x * cos radDir
                                    moveY = x * sin radDir

pivot :: Float -> Object -> Object
pivot x obj = obj { dir = dir obj + x}

grow :: Float -> Object -> Object
grow x obj = obj { size = size obj * x}

wait :: Object -> Object
wait = id

moveX :: Float -> Object -> Object
moveX x obj = obj { posx = x }

moveY :: Float -> Object -> Object
moveY y obj = obj { posy = y }

