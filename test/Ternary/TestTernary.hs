{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ternary.TestTernary where

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.List.FiniteExactNum
import Ternary.List.Aux
import Ternary.Util
import Ternary.Triad
import Ternary.Fiddle

import Control.Monad (liftM, liftM2)
import Control.Arrow (first)
import Data.Monoid (Sum(Sum), Product(Product))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Utils (isCommutable)
import Test.QuickCheck.Classes

instance Arbitrary T1 where
  arbitrary = elements [M, O, P]

instance Arbitrary T2 where
  arbitrary = elements [M2, M1, O0, P1, P2]

instance Arbitrary T4 where
  arbitrary = elements [Ma4, Ma3, Ma2, Ma1, Oa0, Pa1, Pa2, Pa3, Pa4]

instance Arbitrary Triad where
  arbitrary = liftM2 makeTriad arbitrary exponent
    where exponent = fmap getNonNegative arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = liftM Sum arbitrary 

instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = liftM Product arbitrary

-- De facto not unsafe because arbitrary lists are finite:
  
instance Arbitrary FiniteExact where
  arbitrary = liftM2 construct arbitrary exponent
    where construct as p = unsafeFinite (Exact as p)
          exponent = fmap getNonNegative arbitrary

instance EqProp FiniteExact where (=-=) = eq

instance EqProp Triad where (=-=) = eq

-- We use rational numbers as a model for both additive and
-- multiplicative triads.

instance Model Triad Rational where
  model = toRational

instance Model (Sum Triad) (Sum Rational) where
  model (Sum a) = Sum (model a)

instance Model (Product Triad) (Product Rational) where
  model (Product a) = Product (model a)

instance Model FiniteExact Triad where
  model = finiteExactToTriad

instance Model (Sum FiniteExact) (Sum Triad) where
  model (Sum a) = Sum (model a)

instance Model (Product FiniteExact) (Product Triad) where
  model (Product a) = Product (model a)

-- for brevity 
exact :: Integer -> FiniteExact
exact = fromInteger

triad :: Triad
triad = undefined

finiteExact :: FiniteExact
finiteExact = undefined

testTriad :: [TestBatch]
testTriad =
  [first ("Additive Triad - " ++) $ semanticMonoid (Sum triad),
   first ("Multiplicative Triad - " ++) $ semanticMonoid (Product triad),
   first ("Ordered Triad - " ++ ) $ semanticOrd triad]

testTernary :: TestBatch
testTernary =
  ("Basic properties of redundant ternary representation",
   [("Invariant under redundancy", property qcFiddle),
    ("Convert between triad and exact", qcTriadExactConversion),
    ("Add ternary digits", property qcAddT2),
    ("Multiply ternary digits", property qcMultiplyT2),
    ("Combined properties", property qcCombinedPropertiesT2),
    ("Commutative operations", qcCommutativeOpsT2),
    ("Carry and remainder", property qcCarry)])

testAddition :: TestBatch
testAddition = first ("Additive FiniteExact - " ++) $
               semanticMonoid (Sum finiteExact)

testMultiplication :: TestBatch
testMultiplication = first ("Multiplicative FiniteExact - " ++) $
                     semanticMonoid (Product finiteExact)

alternativeTests :: TestBatch
alternativeTests =
  ("Alternative tests - using other ways to generate test data",
   [("Exact integer addition", property qcExactIntegerAddition),
    ("Exact integer multiplication", property qcExactIntegerMultiplication),
    ("Scalar multiplication", property qcScalar)])

qcExactIntegerAddition :: Integer -> Integer -> Bool
qcExactIntegerAddition m n = exact n + exact m == exact (n+m)
 
qcExactIntegerMultiplication :: Integer -> Integer -> Bool
qcExactIntegerMultiplication m n = exact n * exact m == exact (n*m)
 
qcTriadExactConversion = inverse finiteExactToTriad triadToFiniteExact

qcFiddle :: [Bool] -> [Bool] -> [Bool] -> FiniteExact -> Bool
qcFiddle p q r x = fiddleMore p q r x == x

qcAddT2 :: T2 -> T2 -> Bool
qcAddT2 a b = fromT4 (addT2 a b) == fromT2 a + fromT2 b

qcMultiplyT2 :: T2 -> T2 -> Bool
qcMultiplyT2 a b = fromT4 (multiplyT2 a b) == fromT2 a * fromT2 b


qcCombinedPropertiesT2 :: T2 -> Bool
qcCombinedPropertiesT2 a = let na = negateT2 a
  in eq3 (multiplyT2 P2 a) (multiplyT2 M2 na) (addT2 a a) &&
     eq3 (multiplyT2 P1 a) (multiplyT2 M1 na) (addT2 a O0) && 
     multiplyT2 O0 a == Oa0

qcCommutativeOpsT2 :: Property
qcCommutativeOpsT2 = isCommutable addT2 .&&. isCommutable multiplyT2

qcCarry :: T4 -> Bool
qcCarry a = let (c,r) = carry a
  in fromT4 a == 3 * fromT1 c + fromT1 r 

qcScalar :: T2 -> Integer -> Bool
qcScalar a n = scalarFiniteExact a (fromInteger n) == fromInteger (fromT2 a * n)

-- Use an interesting hard-coded example to test the self kernel
-- carry (P1 * P2) = (P,M) ~ (P1,M)

qcSelf :: T2 -> T1 -> T2 -> Bool
qcSelf u v w = take 3 (selfList P1 P2 inp) == out
  where inp = [u, embedT1 v, w]
        out = [P1, addT1 M v, w]

suite = testTriad ++ [testTernary,
                      testAddition,
                      testMultiplication,
                      alternativeTests]
