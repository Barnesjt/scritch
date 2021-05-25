module Test where

import Prelude hiding (LT, GT, EQ)
import Lib

--Some testing values for an object, transformations, and animation seq
testObj :: Object
testObj = Object "x" "Circle" 0 0 0 20 0

tt1, tt2, tt3, tt4 :: TimedTransformation
tt1 = (1, Pivot $ Lit 90)
tt2 = (3, Step $ Lit 10)
tt3 = (1, Grow $ Lit 5)
tt4 = (0, Pivot $ Lit 45)
tt5 = (10, Step $ Lit 15)

tt6 = (0.005, Combine [Pivot $ Lit 3, Step $ Lit 3, Grow $ Lit 1.0007])

as1 :: AnimationSeq
as1 = [tt1, tt2, tt3, tt4, tt5]

as2 :: AnimationSeq
as2 = (0, Move (Lit 250) (Lit 250)) : [(10, Combine [Step (Lit 10), Pivot (Lit 3600)])]

as3 :: AnimationSeq
as3 = repT tt6 5000

as4 :: AnimationSeq
as4 = [(0, Move (Lit 250) (Lit 250)), (1, Step (Lit (-10)))]

repT :: TimedTransformation -> Int -> AnimationSeq
repT t 0 = []
repT t i = t : repT t (i-1)

-- should return 90
testExpr :: Expr Int
testExpr = If (Bin Or (Bin GT (Lit 9) (Lit 10)) (Lit True))
                (Bin Mul (Lit 9) (Lit 10))
                (Bin Sub (Lit 9) (Lit 10))
