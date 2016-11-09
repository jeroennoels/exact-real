module Ternary.List.FiniteExactNum () where

import Ternary.Core.Digit (T2(..))
import Ternary.List.Exact (Exact(Exact))
import Ternary.List.FiniteExact
import Ternary.List.ExactNum

-- Derive the finite version from the infinite one.  Thus we avoid
-- duplication and maximize test coverage.  Note that signum was
-- undefined in the infinite case.

instance Num FiniteExact where
  abs = unsafeLift abs
  signum = safeSignum
  negate = unsafeLift negate
  fromInteger = integralPart . fromInteger
  (+) = addFiniteExact
  (*) = mulFiniteExact


zero, one :: FiniteExact
zero = 0
one = 1

safeSignum :: FiniteExact -> FiniteExact
safeSignum x = let (Exact ds _) = unwrapFinite x in sgn ds
  where sgn :: [T2] -> FiniteExact
        sgn (P2:_) = one
        sgn (P1:_) = one  -- valid only when the list is finite!
        sgn (O0:a) = sgn a
        sgn (M1:_) = negate one          
        sgn (M2:_) = negate one
        sgn [] = zero          

-- The difficulty is to cut off the infinite result at a safe length.
addFiniteExact :: FiniteExact -> FiniteExact -> FiniteExact
addFiniteExact x y = takeFinite cutoff infinite
   where infinite = infiniteExact x + infiniteExact y
         p = offset x
         q = offset y
         s = max p q
         xlen = s-p + finiteLength x
         ylen = s-q + finiteLength y
         cutoff = max xlen ylen + 1

mulFiniteExact :: FiniteExact -> FiniteExact -> FiniteExact
mulFiniteExact x y = takeFinite cutoff infinite
   where infinite = infiniteExact x * infiniteExact y
         cutoff = finiteLength x + finiteLength y + 1
