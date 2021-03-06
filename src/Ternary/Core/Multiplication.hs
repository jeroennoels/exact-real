{-# LANGUAGE FlexibleInstances #-}

module Ternary.Core.Multiplication (
  Triangle, AppliedTriangle, TS,
  TriangleState (..),
  MultiplicationState (..),
  TriangleParam (TriangleParam),
  MulState (MulState),
  scalar, selfTerms,
  initialTS, multKernel, fineStructure) where

import Ternary.Core.Kernel
import Ternary.Core.Digit
import Ternary.Core.Addition

import Control.Arrow (second)

-- Step by step we shall construct an efficient algorithm for exact
-- ternary multiplication.  Iteratively, we develop a series of fully
-- functional multiplication algorithms, where each version becomes a
-- stepping stone towards the next version.  The first iteration is
-- derived from first principles.  Its implementation provides insight
-- because it can be explained, examined and understood.  We call this
-- the fine-structure algorithm, because all the internal details of
-- the construction are explicitly modeled.  Subsequent versions build
-- on previous versions, replacing inefficient data structures with
-- more efficient ones.  These versions can only be explained in terms
-- of their predecessors.  The final version will be about an order of
-- magnitude more efficient in both time and space.

-- See explain.txt for a detailed explanation of the basic algorithm.

scalar :: T2 -> Kernel T2 T2 Sa
scalar a = plus . multiplyT2 a

crossTerms :: T2 -> T2 -> Kernel (T2,T2) T2 ((Sa,Sa),Sa)
crossTerms a b = zipKernelsWith addT2 (scalar b) (scalar a) `serial` plus 

selfTerms :: T2 -> T2 -> Kernel T2 T2 FirstTwoSteps
selfTerms a b = transformFirstTwo (const (embedT1 c)) (addT1 r . coerceT1) 
  where (c,r) = carry (multiplyT2 a b)


newtype TS = TS ((((Sa,Sa),Sa), FirstTwoSteps), Sa)
           deriving (Show, Eq, Ord)
                    
initialTS :: TS
initialTS = TS ((((Sa0,Sa0),Sa0), Step0), Sa0)

stepMatch :: FirstTwoSteps -> TS -> Bool
stepMatch a (TS ((_,b),_)) = a == b 

type Triangle s = Kernel ((T2,T2),T2) T2 s

-- One piece of input (a single digit) comes from the output of the
-- preceding triangle in the chain.  The other piece of input is the
-- same throughout the chain.  More precisely, it remains constant for
-- one state transition of the complete chain.  Here we fix the part
-- that is constant.  See explain.txt for details.

type AppliedTriangle s = Kernel T2 T2 s

data TriangleParam = TriangleParam T2 T2
                   deriving (Show, Eq, Ord)

class TriangleState s where
  initialState :: TriangleParam -> s
  isSecondState :: s -> Bool
  makeTriangle :: TriangleParam -> Triangle s

applyTriangle :: TriangleState s => (T2,T2) -> TriangleParam -> AppliedTriangle s
applyTriangle ab param r state = makeTriangle param (ab,r) state 

buildCircuit :: TriangleParam -> Triangle ((((Sa,Sa),Sa), FirstTwoSteps), Sa)
buildCircuit (TriangleParam a b) =
  zipKernelsWith addT2 (crossTerms a b) (selfTerms a b) `serial` plus

-- The presence of FirstTwoSteps in TS has two consequences.  First,
-- we can never re-enter the initial state.  Second, states that can
-- happen on the second step cannot happen at any other time and vice
-- versa.

instance TriangleState TS where
  initialState = const initialTS
  isSecondState = stepMatch Step1
  makeTriangle param input (TS s) = second TS $! buildCircuit param input s
  
chained :: TriangleState s => (T2,T2) -> [TriangleParam] -> Kernel T2 T2 [s]
chained = chain . applyTriangle

step :: TriangleState s => (T2,T2) -> [TriangleParam] -> [s] -> (T2, [s])
step ab ps = chained ab ps irrelevant
  where irrelevant = undefined :: T2

data MulState s = MulState [TriangleParam] [s]
                deriving Show

-- Notice the "final cons" that adds an initial state to prepare for
-- the next round of chained transitions.
multKernel :: TriangleState s => Kernel (T2,T2) T2 (MulState s)
multKernel ab (MulState ps us) =
  let (out, vs) = step ab ps us
      p = uncurry TriangleParam ab
  in (out, MulState (p:ps) (initialState p:vs))


class MultiplicationState s where
  kernel :: Kernel (T2,T2) T2 s
  initialMultiplicationState :: TriangleParam -> s

instance TriangleState s => MultiplicationState (MulState s) where
  kernel = multKernel
  initialMultiplicationState p = MulState [p] [initialState p]

-- algorithm selector
fineStructure :: MulState TS
fineStructure = undefined
