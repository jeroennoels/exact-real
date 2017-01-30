module Ternary.List.FiniteExact (
  FiniteExact,
  offset, shift, shiftBy, takeFinite, integralPart,
  infiniteExact, finiteLength, unwrapFinite, unsafeFinite,
  unsafeApplyFinite, unsafeLift, truncateLift,
  triadToFiniteExact, finiteExactToTriad,
  finitizeAdd, finitizeMult,
  multiplyAltFS, multiplyAltIE, multiplyAltAL) where

import Data.List (genericLength, genericTake)

import Ternary.Core.Digit
import Ternary.Util.Triad
import Ternary.Util.Misc (Binop)
import Ternary.Core.Multiplication (fineStructure)
import Ternary.Compiler.StateSpace (integerEncoding)
import Ternary.List.Exact hiding (
  multiplyAltIE, multiplyAltFS, multiplyAltAL)
import qualified Ternary.List.Exact as Exact (
  multiplyAltIE, multiplyAltFS, multiplyAltAL)

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

-- Only use the following when you know the given function transforms
-- finite to finite:
unsafeLift :: (Exact -> Exact) -> FiniteExact -> FiniteExact
unsafeLift f (Finite t) = Finite (f t) 

truncateLift :: Int -> (Exact -> Exact) -> FiniteExact -> FiniteExact
truncateLift n f x = takeFinite cut inf
  where inf = f (infiniteExact x)
        cut = n + finiteLength x

-- The caller takes responsibility for ensuring finite data.
unsafeFinite :: Exact -> FiniteExact
unsafeFinite = Finite

-- We do not want to export the constructor of FiniteExact.
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

integralPart :: Exact -> FiniteExact
integralPart exact@(Exact x p) = takeFinite p exact 

-- Now we define the semantics of a finite list of T2 digits.

phi :: [T2] -> Triad
phi (a:as) = div3 (fromT2 a + phi as)
phi [] = 0

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


-- Finally we define finite addition and multiplication in terms of
-- their infinite cousins.  For addition, the difficulty is to cut off
-- the infinite result at a safe length:

finitizeAdd :: Binop Exact -> Binop FiniteExact
finitizeAdd (++) x y = takeFinite cutoff infinite
   where infinite = infiniteExact x ++ infiniteExact y
         p = offset x
         q = offset y
         s = max p q
         xlen = s-p + finiteLength x
         ylen = s-q + finiteLength y
         cutoff = max xlen ylen + 1

finitizeMult :: Binop Exact -> Binop FiniteExact
finitizeMult (**) x y = takeFinite cutoff infinite
   where infinite = infiniteExact x ** infiniteExact y
         cutoff = finiteLength x + finiteLength y + 1

multiplyAltFS :: Binop FiniteExact
multiplyAltFS = finitizeMult $ Exact.multiplyAltFS

multiplyAltIE :: Binop FiniteExact
multiplyAltIE = finitizeMult $ Exact.multiplyAltIE

multiplyAltAL :: Binop FiniteExact
multiplyAltAL = finitizeMult $ Exact.multiplyAltAL
