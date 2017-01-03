module Ternary.Core.Multiplication where

import Ternary.Core.Kernel
import Ternary.Core.Digit
import Ternary.Core.Addition

import Control.Arrow (second)

-- See explain.txt for a detailed explanation of the algorithm. 

scalar :: T2 -> Kernel T2 T2 Sa
scalar a = plus . multiplyT2 a

crossTerms :: T2 -> T2 -> Kernel (T2, T2) T2 ((Sa, Sa), Sa)
crossTerms a b = zipKernelsWith addT2 (scalar b) (scalar a) `serial` plus 

selfTerms :: T2 -> T2 -> Kernel T2 T2 FirstTwoSteps
selfTerms a b = transformFirstTwo (const (embedT1 c)) (addT1 r . coerceT1) 
  where (c,r) = carry (multiplyT2 a b)


newtype TS = TS ((((Sa, Sa), Sa), FirstTwoSteps), Sa)
           deriving (Show, Eq, Ord)
                    
initialTS :: TS
initialTS = TS ((((Sa0, Sa0), Sa0), Step0), Sa0)

stepMatch :: FirstTwoSteps -> TS -> Bool
stepMatch a (TS ((_, b), _)) = a == b 

type Triangle s = Kernel ((T2,T2), T2) T2 s

-- One piece of input (a single digit) comes from the output of the
-- preceding triangle in the chain.  The other piece of input is the
-- same throughout the chain.  More precisely, it remains constant for
-- one state transition of the complete chain.  Here we fix the part
-- that is constant.  See explain.txt for details.

type AppliedTriangle s = Kernel T2 T2 s

data TriangleParam = TriangleParam T2 T2
                   deriving (Show, Eq, Ord)

class TriangleState s where
  initialState :: s
  isSecondState :: s -> Bool
  makeTriangle :: TriangleParam -> Triangle s

applyTriangle :: TriangleState s => (T2,T2) -> TriangleParam -> AppliedTriangle s
applyTriangle ab param r state = makeTriangle param (ab,r) state 

buildCircuit :: TriangleParam -> Triangle ((((Sa, Sa), Sa), FirstTwoSteps), Sa)
buildCircuit (TriangleParam a b) =
  zipKernelsWith addT2 (crossTerms a b) (selfTerms a b) `serial` plus


instance TriangleState TS where
  initialState = initialTS
  isSecondState = stepMatch Step1
  makeTriangle param input (TS s) = second TS $ buildCircuit param input s
  
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
