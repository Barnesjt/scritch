{-# LANGUAGE GADTs #-}

module Anim.AST where

import Prelude hiding (LT, GT, EQ)
import Data.Dynamic

data Expr a where
    -- variable reference
    Var     :: Typeable a => String -> TypeDec a -> Expr a

    -- variable assignment
    Let     :: Typeable a => String -> TypeDec a -> Expr a -> Expr a

    -- Literals
    Lit     :: Show a => a -> Expr a   -- TODO: remove show constraint from Lit, it's only there for testing purposes

    -- unary operatators
    Un      :: Function (a -> b) -> Expr a -> Expr b

    -- binary operators
    Bin     :: Function (a -> b -> c) -> Expr a -> Expr b -> Expr c

    -- conditional
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

    -- retrieve info from an Object (should we represent this using Function?)
    -- that would mean having literal ObjectFields and Literal Objects, so probably no
    Get     :: Object -> ObjectField a -> Expr a



data TypeDec a where
    TyInt   :: TypeDec Int
    TyFloat :: TypeDec Float
    TyBool  :: TypeDec Bool
    TyStr   :: TypeDec String
    TyObj   :: TypeDec Object

    TyUn    :: TypeDec a -> TypeDec b -> TypeDec (Function (a -> b))
    TyBin   :: TypeDec a -> TypeDec b -> TypeDec c -> TypeDec (Function (a -> b -> c))
    TyComp  :: TypeDec (Function (b -> c)) -> TypeDec (Function (a -> b)) -> TypeDec (Function (a -> c))


--Transformation is a minimal set of all possible Transformations.
-- This includes a combine type constructor for compound transformations.

-- Pivot is for rotating from the current angle
-- Move is an absolute move to new coordinates
-- Grow will change the object size by a factor (1.0 means it stays the same)
-- Step will move a distance in the direction it faces
-- Wait does nothing (remain static)
-- Combine sequences multiple transformations, eval'd left to right
data Transformation = Pivot   (Expr Float)
                    | Move    (Expr Float) (Expr Float)
                    | Grow    (Expr Float)
                    | Step    (Expr Float)
                    | Wait
                    | Combine [Transformation]

-- type for all functions
data Function a where
    Add :: Num a => Function (a -> a -> a)
    Mul :: Num a => Function (a -> a -> a)
    Sub :: Num a => Function (a -> a -> a)

    And :: Function (Bool -> Bool -> Bool)
    Or  :: Function (Bool -> Bool -> Bool)

    LT  :: Ord a => Function (a -> a -> Bool)
    GT  :: Ord a => Function (a -> a -> Bool)
    EQ  :: Ord  a => Function (a -> a -> Bool)  -- restricting EQ to Ord instead of just Eq for convenience

    Not :: Function (Bool -> Bool)
    Neg :: Num a => Function (a -> a)

    Compose :: Function (b -> c) -> Function (a -> b) -> Function (a -> c)

op :: Function a -> a
op Add = (+)
op Mul = (*)
op Sub = (-)

op And = (&&)
op Or  = (||)

op LT  = (<)
op GT  = (>)
op EQ  = (==)

op Not = not
op Neg = negate

op (Compose f g) = op f . op g

-- data type of Object field references, for use with Get
data ObjectField a where
    Name :: ObjectField String
    Disp :: ObjectField String
    PosX :: ObjectField Float
    PosY :: ObjectField Float
    PosZ :: ObjectField Float
    Size :: ObjectField Float
    Dir  :: ObjectField Float

-- evaluate an abstract expression -- currently don't know what to do with Tranformations
-- maybe we just won't deal with them here, because they are handled by the doTransform functions
eval :: Expr a -> a
eval (Lit a)     = a
eval (Bin f l r) = op f (eval l) (eval r)
eval (Un f e) = op f (eval e)
eval (If c t e)  = if eval c then eval t else eval e
eval (Get o f) = get f o where
    get :: ObjectField a -> Object -> a
    get Name = name
    get Disp = disp
    get PosX = posx
    get PosY = posy
    get PosZ = posz
    get Size = size
    get Dir  = dir


-- Object is defined with record syntax
-- name is for internal referencing
-- display is meant to be parsed to the displayed object (such as a shape)
-- posx and posy are locations on the screen (0,0 is center for Gloss)
-- posz would be the depth in the scene to render the object (postive on "top")
-- size for the draw size, For a circle this is the radius
-- dir is the direction the object "faces", ie the direction it will step in. (East is 0 degrees)
data Object =
  Object
    { name :: String,
      disp :: String,
      posx :: Float,
      posy :: Float,
      posz :: Float,
      size :: Float,
      dir :: Float
    } deriving (Show)
