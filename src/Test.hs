module Test where

import Lib

--Some testing values for an object, transformations, and animation seq
testObj :: Object
testObj = Object "x" "Circle" 0 0 0 20 0

tt1, tt2, tt3, tt4 :: TimedTransformation
tt1 = (1, Pivot 90)
tt2 = (3, Step 10)
tt3 = (1, Grow 5)
tt4 = (0, Pivot 45)
tt5 = (10, Step 15)

tt6 = (0.005, Combine [Pivot 3, Step 3, Grow 1.0007])

as1 :: AnimationSeq
as1 = [tt1, tt2, tt3, tt4, tt5]

as2 :: AnimationSeq
as2 = (0, Move 250 250) : [(10, Combine [Step 10, Pivot 3600])]

as3 :: AnimationSeq
as3 = repT tt6 5000

as4 :: AnimationSeq
as4 = []

repT :: TimedTransformation -> Int -> AnimationSeq
repT t 0 = []
repT t i = t : repT t (i-1)