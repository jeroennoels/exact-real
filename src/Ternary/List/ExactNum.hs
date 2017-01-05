{-# LANGUAGE ScopedTypeVariables #-}

-- No explicit exports.
-- Typeclass instances are always exported?
module Ternary.List.ExactNum () where

import Ternary.Core.Digit (T2(..), addT2, negateT2)
import Ternary.Core.Addition (plus, Sa(Sa0))
import Ternary.Core.Multiplication
import Ternary.List.Kernel (recurse)
import Ternary.List.Exact

negateDigits :: [T2] -> [T2]
negateDigits = map negateT2
  
absDigits :: [T2] -> [T2]
absDigits [] = []
absDigits (O0:as) = O0 : absDigits as
absDigits x@(P1:_) = x
absDigits x@(P2:_) = x
absDigits x = negateDigits x

instance Num Exact where
  abs = unsafeApplyToDigits absDigits
  signum = error "Exact.signum is undefined" 
  negate = unsafeApplyToDigits negateDigits
  fromInteger = integerToExact 
  (+) = addExact
  (*) = mulExact

add :: [T2] -> [T2] -> [T2]
add x y = recurse plus (zipWith addT2 x y) Sa0

addExact :: Exact -> Exact -> Exact
addExact (Exact x p) (Exact y q) = Exact z (s+1)
  where s = max p q
        z = add (prepend (s-p) x) (prepend (s-q) y) 

mul :: forall s . MultiplicationState s => s -> [T2] -> [T2] -> [T2]
mul _ (x:xs) (y:ys) = recurse kernel (zip xs ys) (init::s)
  where init = initialMultiplicationState (TriangleParam x y)

-- algorithm selection
fineStructure :: MulState TS
fineStructure = undefined

mulExact :: Exact -> Exact -> Exact
mulExact (Exact x p) (Exact y q) = Exact (mul fineStructure x y) (p+q+1)
