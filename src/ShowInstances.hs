{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module ShowInstances where

import AnimationLib
import Prelude hiding (EQ, LT, GT)
import Data.Typeable

instance Show (Expr a) where
    show (Lit x)      = "Lit " ++ show x
    show (Un f e)     = "Un " ++ show f ++ " (" ++ show e ++ ")"
    show (Bin f l r)  = "Bin " ++ show f ++ " (" ++ show l ++ ") (" ++ show r ++ ")"

    show (If c t e)   = "If (" ++ show c ++ ") Then (" ++ show t ++ ") Else (" ++ show e ++ ")"

    show (Get o f)    = "Get (Object " ++ name o ++ ") " ++ show f

-- standalone deriving
deriving instance Show Transformation

instance Show (Function a) where
    show Add           = "Add"
    show Mul           = "Mul"
    show Sub           = "Sub"

    show And           = "And"
    show Or            = "Or"

    show LT            = "LT"
    show GT            = "GT"
    show EQ            = "EQ"

    show Not           = "Not"
    show Neg           = "Neg"

    show (Compose f g) = "(Compose " ++ show f ++ " " ++ show g ++ ")"

instance Show (ObjectField a) where
    show Name = "Name"
    show Disp = "Disp"
    show PosX = "PosX"
    show PosY = "PosY"
    show PosZ = "PosZ"
    show Size = "Size"
    show Dir  = "Dir"
