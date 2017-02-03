module Ternary.Fiddle (fiddleMore) where

import Ternary.Core.Digit (T2(..))
import Ternary.List.FiniteExact (FiniteExact, unsafeApplyFinite, shift)
import Ternary.List.FiniteExactNum ()

-- Fiddle two subsequent digits in a way that preserves semantics.
-- We only fiddle pairs where the first digit is non-negative.
-- Negative cases are handled later, by employing symmetry.
fiddlePair :: (T2,T2) -> (T2,T2)
fiddlePair (P2,M1) = (P1,P2)
fiddlePair (P2,M2) = (P1,P1)
fiddlePair (P1,P2) = (P2,M1)
fiddlePair (P1,P1) = (P2,M2)
fiddlePair (P1,M1) = (O0,P2)
fiddlePair (P1,M2) = (O0,P1)
fiddlePair (O0,P2) = (P1,M1)
fiddlePair (O0,P1) = (P1,M2)
fiddlePair x = x

-- Fiddle on some even positions:
fiddleEven :: [Bool] -> [T2] -> [T2]
fiddleEven (q:qs) (a:b:cs) = if q then u:v:ws else a:b:ws
  where (u,v) = fiddlePair (a,b)
        ws = fiddleEven qs cs
fiddleEven _ x = x

-- The basic fiddle that may act on positive cases in even positions:
fiddle :: [Bool] -> FiniteExact -> FiniteExact
fiddle qs = unsafeApplyFinite (fiddleEven qs)

-- Compose several ways of fiddling, but preserve semantics.
-- Shift to fiddle in odd positions.
-- Employ the symmetry of negation.
fiddleMore :: [Bool] -> [Bool] -> [Bool] -> FiniteExact -> FiniteExact
fiddleMore p q r = negate . fiddle p . negate . fiddle q . shift . fiddle r
