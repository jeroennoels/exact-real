-- No explicit exports.
-- Typeclass instances are always exported?
module Ternary.List.FiniteExactNum () where

import Ternary.Core.Digit (T2(..))
import Ternary.List.Exact (Exact(Exact), addExact, multiplyExact)
import Ternary.List.FiniteExact
import Ternary.List.ExactNum ()

-- Derive the finite version from the infinite one.  Thus we avoid
-- duplication and maximize test coverage.  Note that signum was
-- undefined in the infinite case.

instance Num FiniteExact where
  abs = unsafeLift abs
  signum = safeSignum
  negate = unsafeLift negate
  fromInteger = integralPart . fromInteger
  (+) = finitizeAdd (+)
  (*) = finitizeMult (*)


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
