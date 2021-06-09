module Play.Collision where

import Play.Object

-- This is used to encode a boundry box for the shape. Really just an intermediate type for collision checking
data Boundry
    = Round Point Float        --centerX, centerY, Radius
    | Rectangle Point Point    --minX, minY, maxX, maxY

-- Another convienence type
type Point = (Float, Float)

-- Another shortcut, simple 2d distance formula
distance :: Point -> Point -> Float
distance (x, y) (x', y') = sqrt ((x' - x)**2 + (y' - y)**2)

-- Master function to determines in 2 objects are colliding
testCollide :: Object -> Object -> Bool
testCollide o o' = collideBoundry (getBoundry o) (getBoundry o')

-- For parsing out boundaries
getBoundry :: Object -> Boundry
getBoundry o@Object {shape=Circle} = Round (posx o, posy o) (size o / 2)
getBoundry o@Object {shape=Rect ratio} = Rectangle (x1, y1) (x2, y2)
    where
        halfwidth = size o / 2
        halfheight = (ratio * size o) / 2
        x1 = posx o - halfwidth
        x2 = posx o + halfwidth
        y1 = posy o - halfheight
        y2 = posy o + halfheight

-- The four cases of objects colliding (one case is a repeat)
collideBoundry :: Boundry -> Boundry -> Bool
collideBoundry  (Round (x,y) r)
                (Round (x',y') r')
                = distance (x,y) (x',y') < max r r'
collideBoundry  (Rectangle (minx,miny) (maxx,maxy))
                (Rectangle (minx',miny') (maxx',maxy'))
                = not (maxy >= minx' || maxx' >= minx || maxy >= miny' || maxy' >= miny)
collideBoundry  (Round (x,y) r)
                (Rectangle (minx,miny) (maxx,maxy))
                = dX < r + r
                where nearest cx x' x'' = max x' (min cx x'')
                      nearX = nearest x minx maxx
                      nearY = nearest y miny maxy
                      dX = distance (nearX, nearY) (x, y)
collideBoundry b b' = collideBoundry b' b