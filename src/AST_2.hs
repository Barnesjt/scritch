module AST_2 where

type Prog = ([Object], [Stmt])

-- we're just applying functions to arguments
data Stmt = App Function Stmt | Base Basic
    deriving (Eq, Show)

-- some basic data types
data Basic = Obj String
           | Int Int
           | Bool Bool
           | Str String
    deriving (Eq, Show)

data Function = Move Float Float -- Move a number of pixels in x and y directions
    deriving (Eq, Show)

-- TODO figure out what objects are, and what to call them
-- for now we'll just keep track of position and a name
type Object = (String, (Float, Float)) -- placeholder

-- smart constructors
obj :: String -> Stmt
obj = Base . Obj

int :: Int -> Stmt
int = Base . Int

bool :: Bool -> Stmt
bool = Base . Bool

str :: String -> Stmt
str = Base . Str

-- Random Thoughts --

-- re: argument lists, typechecking - it's possible we don't need to do any of that in the first place,
-- i.e. we could enforce tpye correctness in the visual object lang

-- control flow constructs: sequencing, synchronizing, loops? As it stand we could make these sort of statements regular functions

-- how important is it, in an educational context, that functions are truly first class? could compromise this property if it makes implementation significantly easier
