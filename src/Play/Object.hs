module Play.Object where

import Data.Map.Strict as M ( Map, fromList )

-- Object is defined with a record syntax
data Object =
  Object {
      shape :: Shape,   -- shape defines the appearance of the object
      visible :: Bool,  -- whether to draw the image or not
      posx :: Float,    -- posx and posy are locations on the screen 
      posy :: Float,
      posz :: Float,    -- posz would be the depth in the scene to render the object
      size :: Float,    -- size is the draw size
      dir :: Float      -- direction determines which way the objects faces and steps (East is 0 degrees)
    } deriving (Show)

type ObjMap = M.Map String Object

--used to encode how the shape is drawn & collided with
data Shape
    = Circle        --No additional data needed. diameter = size. Centered at (posx,posy)
    | Rect Float    --Ratio of height to width. width = object size. height = ratio * size. Centered at (posx,posy)
    deriving (Show)

--convenience function for computing radians from degrees (which is what we store)
--  radians are needed for sin/cos for step
rad :: Float -> Float
rad x = x * (pi / 180.0)

-- Object Transformations
-- Step, move x distance along it's direction vector
step :: Float -> Object -> Object
step x obj = obj { posx = posx obj + moveX, posy = posy obj + moveY}
                              where radDir = rad $ dir obj
                                    moveX = x * cos radDir
                                    moveY = x * sin radDir

--Pivot, turn x degrees
pivot :: Float -> Object -> Object
pivot x obj = obj { dir = dir obj + x}

--Grow, increase x times. 2 is double, .5 is half
grow :: Float -> Object -> Object
grow x obj = obj { size = size obj * x}

--Wait, does nothing (just id)
wait :: Object -> Object
wait = id

--MoveX, moves object to this value on the x axis
moveX :: Float -> Object -> Object
moveX x obj = obj { posx = x }

--MoveY, moves object to this value on the y axis
moveY :: Float -> Object -> Object
moveY y obj = obj { posy = y }

