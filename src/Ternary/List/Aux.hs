-- Just to support unit testing.

module Ternary.List.Aux where

import Ternary.Core.Digit
import Ternary.Core.Addition
import Ternary.Core.Multiplication
import Ternary.Core.Kernel
import Ternary.List.Kernel
import Ternary.List.Exact
import Ternary.List.FiniteExact

scalarExact :: T2 -> Exact -> Exact
scalarExact a (Exact x p) = Exact y (p+1)
  where y = recurse (scalar a) x Sa0

scalarFiniteExact :: T2 -> FiniteExact -> FiniteExact
scalarFiniteExact a = truncateLift 1 (scalarExact a)

selfList :: T2 -> T2 -> [T2] -> [T2]
selfList a b x = recurse (selfTerms a b) x Step0
