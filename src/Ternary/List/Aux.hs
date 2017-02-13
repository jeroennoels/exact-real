-- Just to support unit testing.
module Ternary.List.Aux where

import Ternary.Core.Digit (T2)
import Ternary.Core.Addition (Sa(Sa0))
import Ternary.Core.Multiplication (scalar, selfTerms)
import Ternary.Core.Kernel (Kernel, FirstTwoSteps(Step0))
import Ternary.List.Kernel (recurse)
import Ternary.List.Exact (Exact(Exact))
import Ternary.List.FiniteExact (FiniteExact, truncateLift)


scalarExact :: T2 -> Exact -> Exact
scalarExact a (Exact x p) = Exact y (p+1)
  where y = recurse (scalar a) x Sa0

scalarFiniteExact :: T2 -> FiniteExact -> FiniteExact
scalarFiniteExact a = truncateLift 1 (scalarExact a)

selfList :: T2 -> T2 -> [T2] -> [T2]
selfList a b x = recurse (selfTerms a b) x Step0
