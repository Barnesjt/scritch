{-# LANGUAGE ExistentialQuantification #-}

module Store where

import Control.Monad.Except   (ExceptT,MonadError,runExceptT,throwError)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Reader   (MonadReader,ask,local)
import Control.Monad.RWS      (RWST,runRWST)
import Control.Monad.State    (MonadState,get,put)
import Control.Monad.Writer   (MonadWriter,tell)
import Data.Functor.Identity  (Identity)

import Data.Dynamic
import Type.Reflection

import Data.Map (Map)
import qualified Data.Map as Map

import AnimationLib

type Ref a = (String, TypeDec a)

type State = Map.Map String Dynamic

x = toDyn (9 :: Int)
y = fromDyn x "error"


put :: Typeable a => String -> a -> State -> State
put v x = Map.insert v $ toDyn x

get :: Typeable a => String -> TypeDec a -> State -> Maybe a
get v t s = case Map.lookup v s of
    Just d -> fromDynamic d
    Nothing -> Nothing

test = Map.insert "y" (toDyn "hello") $ Map.insert "x" (toDyn (9 :: Int)) Map.empty
