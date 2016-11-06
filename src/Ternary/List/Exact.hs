module Ternary.List.Exact where

import Ternary.Core.Digit

import Ternary.Util (assertNonNegative)
import Data.List (genericReplicate)


prepend :: Integral n => n -> [T2] -> [T2]
prepend n as = genericReplicate nn O0 ++ as
  where nn = assertNonNegative "Exact.prepend" n 

-- semantics of (Exact [] n) is 3^n
-- semantics of (Exact [P1] 0) is 1/3
        
data Exact = Exact [T2] Integer deriving Show

-- only to be used with an argument that transforms infinite lists:

unsafeApplyToDigits :: ([T2] -> [T2]) -> Exact -> Exact
unsafeApplyToDigits f (Exact x p) = Exact (f x) p
