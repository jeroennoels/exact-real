module Ternary.Core.Multiplication where

import Ternary.Core.Kernel
import Ternary.Core.Digit
import Ternary.Core.Addition

scalar :: T2 -> Kernel T2 T2 Sa
scalar a = plus . multiplyT2 a

cross :: T2 -> T2 -> Kernel (T2, T2) T2 ((Sa, Sa), Sa)
cross a b = zipKernelsWith addT2 (scalar b) (scalar a) `serial` plus 

-- I guess the pair (r,c) below will be recomputed many times.  This
-- could be avoided using closures.  But currently I do not want to
-- hide any parameters or state inside closures. This would make it
-- more difficult to understand the time and space complexity of the
-- algorithms.

self :: T2 -> T2 -> Kernel T2 T2 FirstTwoSteps
self a b = transformFirstTwo (const (embedT1 c)) (addT1 r . coerceT1) 
  where (c,r) = carry (multiplyT2 a b)


type TriangleState = ((((Sa, Sa), Sa), FirstTwoSteps), Sa)

initialTriangleState :: TriangleState
initialTriangleState = ((((Sa0, Sa0), Sa0), Step0), Sa0)

type Triangle = Kernel ((T2,T2), T2) T2 TriangleState

type AppliedTriangle = Kernel T2 T2 TriangleState

data TriangleParam = TriangleParam T2 T2


makeTriangle :: TriangleParam -> Triangle
makeTriangle (TriangleParam a b) =
  zipKernelsWith addT2 (cross a b) (self a b) `serial` plus

-- One piece of input (a single digit) comes from the output of the
-- preceding triangle in the chain.  The other piece of input is the
-- same throughout the chain.  More precisely, it remains constant for
-- one state transition of the complete chain.  Here we fix the part
-- that is constant.

applyTriangle :: (T2,T2) -> TriangleParam -> AppliedTriangle
applyTriangle ab p r state = makeTriangle p (ab, r) state   

chained :: (T2, T2) -> [TriangleParam] -> Kernel T2 T2 [TriangleState]
chained = chain . applyTriangle

data MulState = MulState [TriangleParam] [TriangleState]

-- I should turn MulState into a more abstract data type.  Currently
-- we cannot simply swap one kernel implementation for another one.

step :: (T2,T2) -> [TriangleParam] -> [TriangleState] -> (T2, [TriangleState])
step ab ps = unsafeIgnoreInput $ chained ab ps

multKernel :: Kernel (T2,T2) T2 MulState
multKernel ab (MulState ps us) =
  let (out, vs) = step ab ps us
      p = uncurry TriangleParam $ ab
  in (out, MulState (p:ps) (initialTriangleState:vs))
