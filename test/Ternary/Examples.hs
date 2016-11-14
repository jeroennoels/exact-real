module Ternary.Examples where

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.List.FiniteExactNum
import Ternary.Triad

x = Exact [P1,O0,undefined] 0
y = Exact [P2,P2,undefined] 0
z = Exact (cycle ds) 0 where ds = [M2,M1,O0,P1,P2]

--  x+x
--  x*x
--  y*y
                             
z2 = z * z
as = streamDigits z2
slow = take 400 as
fast = take 401 as -- incremental computation

a,b :: Integer
a = 5458086354678022563255767332088955423567
b = 686633208503168754901134087608765434678992253678865

c,d :: Exact
c = fromInteger a
d = fromInteger b

trunc :: FiniteExact
trunc = takeFinite 200 (c*d)

cd :: Triad
cd = finiteExactToTriad trunc

check = let u = a*b
            v = triadNumerator cd
        in print u >> print v >> print (u == v)
