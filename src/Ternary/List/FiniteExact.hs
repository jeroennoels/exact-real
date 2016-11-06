module Ternary.List.FiniteExact (
  FiniteExact, offset,
  shift, shiftBy, takeFinite,
  infiniteExact, finiteLength,
  unwrapFinite, unsafeFinite,
  unsafeApplyFinite, unsafeLift, truncateLift,
  triadToFiniteExact, finiteExactToTriad) where

import Ternary.Core.Digit
import Ternary.Core.Semantics
import Ternary.List.Exact

import Ternary.Triad
import Ternary.Util (digits)
import Data.List (genericLength, genericTake)

-- The main reason for having a dedicated FiniteExact type is that
-- QuickCheck tests will give us finite lists as test data.  So we
-- need to work effectively with such finite inputs and this is
-- surprisingly non-trivial: when we compute the sum of two numbers
-- represented by finite lists of different lengths, we must carefully
-- pad with extra zeros.

newtype FiniteExact = Finite Exact deriving Show

infiniteExact :: FiniteExact -> Exact
infiniteExact (Finite (Exact x p)) = Exact (x ++ repeat O0) p

unsafeApplyFinite :: ([T2] -> [T2]) -> FiniteExact -> FiniteExact
unsafeApplyFinite f (Finite (Exact x p)) = Finite (Exact (f x) p)

-- when you know the given function transforms finite to finite
unsafeLift :: (Exact -> Exact) -> FiniteExact -> FiniteExact
unsafeLift f (Finite t) = Finite (f t) 

truncateLift :: Int -> (Exact -> Exact) -> FiniteExact -> FiniteExact
truncateLift n f x = takeFinite cut inf
  where inf = f (infiniteExact x)
        cut = n + finiteLength x

-- caller takes responsibility for ensuring finite data
unsafeFinite :: Exact -> FiniteExact
unsafeFinite = Finite

unwrapFinite :: FiniteExact -> Exact
unwrapFinite (Finite x) = x

shift :: FiniteExact -> FiniteExact
shift (Finite (Exact x p)) = Finite (Exact (O0:x) (p+1))

shiftBy :: Integral a => a -> FiniteExact -> FiniteExact
shiftBy n = unsafeApplyFinite (prepend n)

finiteLength :: Integral i => FiniteExact -> i
finiteLength (Finite (Exact x _)) = genericLength x

offset :: FiniteExact -> Integer
offset (Finite (Exact _ p)) = p
                                      
takeFinite :: Integral i => i -> Exact -> FiniteExact
takeFinite n (Exact x p) = Finite $ Exact (genericTake n x) p
                                      
-- Now we define the semantics of a finite list of T2 digits.

phi :: [T2] -> Triad
phi (a:as) = div3 (fromT2 a + phi as)
phi [] = 0

-- guaranteed to produce a finite list
digitsT2 :: Integer -> [T2]
digitsT2 k | k >= 0 = convert k
           | otherwise = map negateT2 (convert (-k))
  where convert = map toT2 . digits 3

finiteExactToTriad :: FiniteExact -> Triad
finiteExactToTriad (Finite (Exact x p)) = exp3 p * phi x

triadToFiniteExact :: Triad -> FiniteExact
triadToFiniteExact t
  | n >= 0 = Finite $ Exact x n
  | n < 0  = Finite $ Exact (prepend (-n) x) 0
  where x = digitsT2 (triadNumerator t)
        n = genericLength x - triadExponent t


instance Eq FiniteExact where
  x == y = finiteExactToTriad x == finiteExactToTriad y
