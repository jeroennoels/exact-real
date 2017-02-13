module Ternary.Util.Triad (
  Triad, makeTriad, triadNumerator, triadExponent,
  reduceTriad, div3, mul3, exp3) where

import Ternary.Util.Misc (makeRational, assertNonNegative)

-- Semantically (Triad n p) is the rational number n/3^p.
-- The exponent p is assumed to be non-negative.

data Triad = Triad Integer Integer deriving Show

-- Inside this module, triad construction can be manually verified to
-- produce and preserve non-negative exponents.  The Triad constructor
-- is not exported, so other modules have to pass the following check:

makeTriad :: Integer -> Integer -> Triad
makeTriad n p = Triad n (assertNonNegative "Ternary.Triad (makeTriad)" p)

triadNumerator :: Triad -> Integer
triadNumerator (Triad n _) = n

triadExponent :: Triad -> Integer
triadExponent (Triad _ p) = p


instance Eq Triad where
  Triad n p == Triad m q = n * 3^q == m * 3^p

instance Ord Triad where
  compare (Triad n p) (Triad m q) = compare (n * 3^q) (m * 3^p)

instance Real Triad where
  toRational (Triad n p) = makeRational n (3^p)

instance Num Triad where
  abs (Triad n p) = Triad (abs n) p
  signum (Triad n _) = Triad (signum n) 0
  fromInteger n = Triad n 0
  negate (Triad n p) = Triad (negate n) p
  Triad n p * Triad m q = reduce (n*m) (p+q)
  Triad n p + Triad m q = reduce k s
    where s = max p q
          k = n * 3^(s-p) + m * 3^(s-q)

-- Find the simplest representation of a triad:
reduce :: Integer -> Integer -> Triad
reduce n 0 = Triad n 0
reduce n p = if rem == 0
             then reduce quot (p-1)
             else Triad n p
  where (quot,rem) = quotRem n 3

reduceTriad :: Triad -> Triad
reduceTriad (Triad n p) = reduce n p

div3 :: Triad -> Triad
div3 (Triad n p) = Triad n (p+1)

mul3 :: Triad -> Triad
mul3 (Triad n 0) = fromInteger (3*n)
mul3 (Triad n p) = makeTriad n (p-1)

-- avoid large numerators when possible
exp3 :: Integer -> Triad
exp3 k | k < 0 = Triad 0 (-k)
       | otherwise = fromInteger (3^k)
