module AST where

type Prog = [Stmt]

-- we're just applying functions to arguments
data Stmt = App Function [Function]
    deriving (Eq, Show)

-- some basic data types
data Basic = Obj Object
           | Int Int
           | Bool Bool
           | Str String
    deriving (Eq, Show)

data Function = Base Basic -- even basic data is a function (of 0 arguments probably)
              | Op Operation -- not sure about this
    deriving (Eq, Show)

data Operation = Move -- right now there is no way of enforcing the number or type of function arguments
    deriving (Eq, Show)

-- TODO figure out what objects are, and what to call them
-- for now we'll just keep track of position
type Object = (Int, Int) -- placeholder

-- smart constructors
obj :: (Int, Int) -> Function
obj = Base . Obj

int :: Int -> Function
int = Base . Int

bool :: Bool -> Function
bool = Base . Bool

str :: String -> Function
str = Base . Str

testProg :: Prog
testProg = [App (Op Move) [obj (0, 0), int 10]]


-- Random Thoughts --

-- re: argument lists, typechecking - it's possible we don't need to do any of that in the first place,
-- i.e. we could enforce tpye correctness in the visual object lang

-- control flow constructs: sequencing, synchronizing, loops? As it stand we could make these sort of statements regular functions

-- how important is it, in an educational context, that functions are truly first class? could compromise this property if it makes implementation significantly easier
