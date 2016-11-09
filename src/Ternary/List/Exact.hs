module Ternary.List.Exact where

import Ternary.Core.Digit
import Ternary.Util (assertNonNegative, digits)
import Data.List (genericReplicate, genericLength)

prepend :: Integral n => n -> [T2] -> [T2]
prepend n as = genericReplicate nn O0 ++ as
  where nn = assertNonNegative "Ternary.List.Exact (prepend)" n 

-- semantics of (Exact [] n) is 3^n
-- semantics of (Exact [P1] 0) is 1/3
        
data Exact = Exact [T2] Integer deriving Show


-- Only to be used with an argument that transforms infinite lists:
unsafeApplyToDigits :: ([T2] -> [T2]) -> Exact -> Exact
unsafeApplyToDigits f (Exact x p) = Exact (f x) p

-- This is guaranteed to produce a finite list.
digitsT2 :: Integer -> [T2]
digitsT2 k | k >= 0 = convert k
           | otherwise = map negateT2 (convert (-k))
  where convert = map toT2 . digits 3

integerToExact :: Integer -> Exact
integerToExact n = Exact (x ++ repeat O0) (genericLength x)
  where x = digitsT2 n
