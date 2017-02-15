{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.List.Exact where

import Data.List (genericReplicate, genericLength)

import Ternary.Util.Misc (assertNonNegative, digits, Binop)
import Ternary.Core.Digit
import Ternary.Core.Addition (plus, Sa(Sa0))
import Ternary.Core.Multiplication
import Ternary.List.Kernel (recurse)
import Ternary.Compiler.StateSpace (integerEncoding)
import Ternary.Compiler.ArrayState (arrayLookup, arrayState)


prepend :: Integral n => n -> [T2] -> [T2]
prepend n as = genericReplicate nn O0 ++ as
  where nn = assertNonNegative "Ternary.List.Exact (prepend)" n

-- semantics of (Exact [P1] n) is 1/3 * 3^n

data Exact = Exact [T2] Integer deriving Show

streamDigits :: Exact -> [T2]
streamDigits (Exact x _) = x

-- Only to be used with an argument that transforms infinite lists:

unsafeApplyToDigits :: ([T2] -> [T2]) -> Exact -> Exact
unsafeApplyToDigits f (Exact x p) = Exact (f x) p

-- This is guaranteed to produce a finite list:

digitsT2 :: Integer -> [T2]
digitsT2 k | k >= 0 = convert k
           | otherwise = map negateT2 (convert (-k))
  where convert = map toT2 . digits 3

integerToExact :: Integer -> Exact
integerToExact n = Exact (x ++ repeat O0) (genericLength x)
  where x = digitsT2 n

-- In the following section, we define addition and multiplication of
-- exact numbers.

add :: Binop [T2]
add x y = recurse plus (zipWith addT2 x y) Sa0

addExact :: Binop Exact
addExact (Exact x p) (Exact y q) = Exact z (s+1)
  where s = max p q
        z = add (prepend (s-p) x) (prepend (s-q) y)

-- The purpose of the MultiplicationState argument is to select a
-- particular kernel implementation.  Only its type matters, the value
-- will not be forced.

multiply :: forall s . MultiplicationState s => s -> Binop [T2]
multiply _ (x:xs) (y:ys) = recurse kernel (zip xs ys) (init::s)
  where init = initialMultiplicationState (TriangleParam x y)

multiplyExact :: MultiplicationState s => s -> Binop Exact
multiplyExact alg (Exact x p) (Exact y q) = Exact (multiply alg x y) (p+q+1)


multiplyAltFS :: Binop Exact
multiplyAltFS = multiplyExact fineStructure

multiplyAltIE :: Binop Exact
multiplyAltIE = multiplyExact integerEncoding

multiplyAltAL :: Binop Exact
multiplyAltAL = multiplyExact arrayLookup

multiplyAltAS :: Binop Exact
multiplyAltAS = multiplyExact arrayState
