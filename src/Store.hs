module Store where

import Data.Dynamic

import Data.Map (Map)
import qualified Data.Map as Map

import AnimationLib

type Ref a = (String, TypeDec a)

type State = Map.Map String Dynamic

x = toDyn (9 :: Int)
y = fromDyn x "error"


insert :: Typeable a => String -> a -> State -> State
insert v x = Map.insert v $ toDyn x

query :: Typeable a => String -> TypeDec a -> State -> Maybe a
query v t s = case Map.lookup v s of
    Just d -> fromDynamic d
    Nothing -> Nothing

test = insert "y" "hello" $ insert "x" (9.0 :: Float) Map.empty
