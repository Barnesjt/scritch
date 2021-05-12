module Test where

import Lib

testObj :: Object
testObj = Object "x" "Circle" 0 0 0 20 0

tt1, tt2, tt3, tt4 :: TimedTransformation
tt1 = (1, Pivot 90)
tt2 = (3, Step 5)
tt3 = (1, Wait)
tt4 = (0, Pivot 45)
tt5 = (10, Step 5)

as1 :: AnimationSeq
as1 = [tt1, tt2, tt3, tt4, tt5]