-- typeclass instances are always exported?
module Ternary.List.TernaryNum () where

import Ternary.Core.Digit
import Ternary.Core.Addition
import Ternary.Core.Multiplication
import Ternary.List.Kernel
import Ternary.List.Exact
import Ternary.List.FiniteExact

negateDigits :: [T2] -> [T2]
negateDigits = map negateT2
  
absDigits :: [T2] -> [T2]
absDigits [] = []
absDigits (O0:as) = O0 : absDigits as
absDigits x@(P1:_) = x
absDigits x@(P2:_) = x
absDigits x = negateDigits x   

-- signum is undefined on infinite representations of zero

safeSignum :: FiniteExact -> FiniteExact
safeSignum x = let (Exact ds _) = unwrapFinite x in sgn ds
  where sgn :: [T2] -> FiniteExact
        sgn (P2:_) = one
        sgn (P1:_) = one
        sgn (O0:a) = sgn a
        sgn (M1:_) = negate one          
        sgn (M2:_) = negate one
        sgn [] = zero          

instance Num Exact where
  abs = unsafeApplyToDigits absDigits
  signum = error "Exact.signum is undefined" 
  negate = unsafeApplyToDigits negateDigits
  fromInteger = infiniteExact . fromInteger
  (+) = addExact
  (*) = mulExact

-- Preferably derive the finite version from the infinite one.
-- Thus we avoid duplication and maximize test coverage.
-- In the case of fromInteger, it is more natural the other way around.

instance Num FiniteExact where
  abs = unsafeLift abs
  signum = safeSignum
  negate = unsafeLift negate
  fromInteger = triadToFiniteExact . fromInteger
  (+) = addFiniteExact
  (*) = mulFiniteExact

add :: [T2] -> [T2] -> [T2]
add x y = recurse plus (zipWith addT2 x y) Sa0

addExact :: Exact -> Exact -> Exact
addExact (Exact x p) (Exact y q) = Exact z (s+1)
  where s = max p q
        z = add (prepend (s-p) x) (prepend (s-q) y) 

-- the difficulty is to cut off the infinite result at a safe length
addFiniteExact :: FiniteExact -> FiniteExact -> FiniteExact
addFiniteExact x y = takeFinite cutoff infinite
   where infinite = infiniteExact x + infiniteExact y
         p = offset x
         q = offset y
         s = max p q
         xlen = s-p + finiteLength x
         ylen = s-q + finiteLength y
         cutoff = max xlen ylen + 1

zero, one :: FiniteExact
zero = 0
one = 1

mul :: [T2] -> [T2] -> [T2]
mul (x:xs) (y:ys) = recurse multKernel (zip xs ys) init
  where init = MulState [TriangleParam x y] [initialTriangleState]

mulExact :: Exact -> Exact -> Exact
mulExact (Exact x p) (Exact y q) = Exact (mul x y) (p+q+1)

mulFiniteExact :: FiniteExact -> FiniteExact -> FiniteExact
mulFiniteExact x y = takeFinite cutoff infinite
   where infinite = infiniteExact x * infiniteExact y
         cutoff = finiteLength x + finiteLength y + 1
