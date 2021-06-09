{-# LANGUAGE GADTs #-}

module Anim.Eval where

import Control.Monad
import Data.Dynamic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Anim.AST

import Prelude hiding (LT, GT, EQ)
import Data.Typeable

type Ref a = (String, TypeDec a)

type Store = Map.Map String Dynamic

insert :: Typeable a => String -> a -> Store -> Store
insert v x = Map.insert v $ toDyn x

-- typedec is used by haskell for type inference
query :: Typeable a => String -> TypeDec a -> Store -> Maybe a
query v t s = case Map.lookup v s of
    Just d -> fromDynamic d
    Nothing -> Nothing


newtype State a = S (Store -> (Maybe a, Store))

instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance Monad State where
  return x  = S (\s -> (Just x, s))

  S f >>= g = S (\s -> case f s of
      (Just x,  s') -> let (S h) = g x in h s'
      (Nothing, s') -> (Nothing, s'))


-- primitives --
get :: State Store
get = S (\s -> (Just s, s))

put :: Store -> State ()
put s = S (\_ -> (Just (), s))

empty :: State a
empty = S (\s -> (Nothing, s))


-- monadic eval
evalM :: Expr a -> State a
evalM (Lit x) = return x
evalM (Bin f l r) = do
    l' <- evalM l
    r' <- evalM r
    return $ op f l' r'
evalM (Un f e) = do
    e' <- evalM e
    return $ op f e'
evalM (If c t e) = do
    c' <- evalM c
    r <- evalM (if c' then t else e)
    return r
evalM (Var v t) = do
    s <- get
    case query v t s of
        Just x -> return x
        Nothing -> empty
evalM (Let v t e) = do
    s <- get
    x <- evalM e
    put (insert v x s)
    return x


runState :: State a -> Store -> (Maybe a, Store)
runState (S f) s = f s

runEval :: Expr a -> (Maybe a, Store)
runEval e = runState (evalM e) Map.empty


data Program where
    Base :: Expr a -> Program
    Cons :: Expr a -> Program -> Program

evalProg :: Program -> State ()
evalProg (Base e) = do
    evalM e
    return ()
evalProg (Cons e p) = do
    evalM e
    evalProg p



evalP :: Program -> (Maybe (), Store)
evalP p = runState (evalProg p) Map.empty

testMap :: Store
testMap = insert "x" (9 :: Int) $ insert "y" ("hello") $ insert "z" (True) $ Map.empty
