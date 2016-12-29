module Ternary.Core.Multiplication where

import Ternary.Core.Kernel
import Ternary.Core.Digit
import Ternary.Core.Addition

import Control.Arrow (second)

-- See explain.txt for a detailed explanation of the algorithm. 

scalar :: T2 -> Kernel T2 T2 Sa
scalar a = plus . multiplyT2 a

cross :: T2 -> T2 -> Kernel (T2, T2) T2 ((Sa, Sa), Sa)
cross a b = zipKernelsWith addT2 (scalar b) (scalar a) `serial` plus 

-- Perhaps the pair (c,r) below will be recomputed many times if the
-- compiler fails to optimize this.  We worry about this later.

self :: T2 -> T2 -> Kernel T2 T2 FirstTwoSteps
self a b = transformFirstTwo (const (embedT1 c)) (addT1 r . coerceT1) 
  where (c,r) = carry (multiplyT2 a b)


newtype TS = TS ((((Sa, Sa), Sa), FirstTwoSteps), Sa)
           deriving (Show, Eq, Ord)
                    
initialTS :: TS
initialTS = TS ((((Sa0, Sa0), Sa0), Step0), Sa0)

type Triangle s = Kernel ((T2,T2), T2) T2 s

-- One piece of input (a single digit) comes from the output of the
-- preceding triangle in the chain.  The other piece of input is the
-- same throughout the chain.  More precisely, it remains constant for
-- one state transition of the complete chain.  Here we fix the part
-- that is constant.  See explain.txt for details.

type AppliedTriangle s = Kernel T2 T2 s

data TriangleParam = TriangleParam T2 T2

class TriangleState s where
  applyTriangle :: (T2,T2) -> TriangleParam -> AppliedTriangle s
  initialState :: s

makeTriangle :: TriangleParam -> Triangle ((((Sa, Sa), Sa), FirstTwoSteps), Sa)
makeTriangle (TriangleParam a b) =
  zipKernelsWith addT2 (cross a b) (self a b) `serial` plus

instance TriangleState TS where
  applyTriangle ab p r (TS s) = second TS $ makeTriangle p (ab,r) s
  initialState = initialTS
  
chained :: TriangleState s => (T2, T2) -> [TriangleParam] -> Kernel T2 T2 [s]
chained = chain . applyTriangle

data MulState s = MulState [TriangleParam] [s]

step :: TriangleState s => (T2,T2) -> [TriangleParam] -> [s] -> (T2, [s])
step ab ps = unsafeIgnoreInput $ chained ab ps

multKernel :: TriangleState s => Kernel (T2,T2) T2 (MulState s)
multKernel ab (MulState ps us) =
  let (out, vs) = step ab ps us
      p = uncurry TriangleParam $ ab
  in (out, MulState (p:ps) (initialState:vs))
