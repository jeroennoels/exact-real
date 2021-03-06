{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ternary.TestTernary where

import Ternary.Core.Digit
import Ternary.Core.Normalize
import Ternary.Core.Multiplication (MultiplicationState(kernel))

import Ternary.List.Exact (Exact(Exact))
import Ternary.List.FiniteExact
import Ternary.List.FiniteExactNum ()
import Ternary.List.Aux (selfList, scalarFiniteExact)
import Ternary.QuickCheckUtil (quickSuite)
import Ternary.Arbitraries
import Ternary.Util.Misc (Binop, eq3, zeroIndexedArray)
import Ternary.Util.Triad (Triad, makeTriad)
import Ternary.Fiddle
import Ternary.TestKernel (qcChain)
import Ternary.Compiler.ArrayState (MulStateAL(..), MulStateAS(..))
import Ternary.Compiler.ArrayLookup (
  initialPoints, normalPointBounds, secondPointBounds)

import Control.Monad (liftM, liftM2, liftM3)
import Control.Arrow (first, second)
import Data.Monoid (Sum(Sum), Product(Product))

import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Test.QuickCheck.Utils (isCommutable)
import Test.QuickCheck.Classes (semanticMonoid, semanticOrd)


instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = liftM Sum arbitrary

instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = liftM Product arbitrary

instance Arbitrary MulStateAL where
  arbitrary = liftM3 combine i s n
    where combine a b cs = MulStateAL (a:b:cs)
          i = elements initialPoints 
          s = choose secondPointBounds
          n = listOf (choose normalPointBounds)
    
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

qcIntegerAddition :: Integer -> Integer -> Bool
qcIntegerAddition m n = exact n + exact m == exact (n+m)

qcIntegerMultiplication :: Binop FiniteExact -> Integer -> Integer -> Bool
qcIntegerMultiplication (**) m n = exact n ** exact m == exact (n*m)

qcTriadExactConversion = inverse finiteExactToTriad triadToFiniteExact

qcFiddle :: [Bool] -> [Bool] -> [Bool] -> FiniteExact -> Bool
qcFiddle p q r x = fiddleMore p q r x == x

qcAddT2 :: T2 -> T2 -> Bool
qcAddT2 a b = fromT4 (addT2 a b) == fromT2 a + fromT2 b

qcMultiplyT2 :: T2 -> T2 -> Bool
qcMultiplyT2 a b = fromT4 (multiplyT2 a b) == fromT2 a * fromT2 b

-- Verify simple algebraic properties such as 2*a = (-2)*(-a) = a+a
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
qcScalar a n = scalarFiniteExact a (exact n) == exact (fromT2 a * n)

-- Use an interesting hard-coded example to test the self kernel
-- carry (P1 * P2) = (P,M) ~ (P1,M)
qcSelf :: T2 -> T1 -> T2 -> Bool
qcSelf u v w = take 3 (selfList P1 P2 inp) == out
  where inp = [u, embedT1 v, w]
        out = [P1, addT1 M v, w]

qcArrayLookupVersusArrayState :: (T2,T2) -> MulStateAL -> Bool
qcArrayLookupVersusArrayState input state =
  second toArrayState (kernel input state) == kernel input (toArrayState state)
  
toArrayState :: MulStateAL -> MulStateAS
toArrayState (MulStateAL us) = MulStateAS (zeroIndexedArray us)


-- Monoid selector
triad :: Triad
triad = undefined

-- Monoid selector
finiteExact :: FiniteExact
finiteExact = undefined

testTriad :: [TestBatch]
testTriad =
  [first ("Additive Triad - " ++) $ semanticMonoid (Sum triad),
   first ("Multiplicative Triad - " ++) $ semanticMonoid (Product triad),
   first ("Ordered Triad - " ++ ) $ semanticOrd triad]

testBasicTernary :: TestBatch
testBasicTernary =
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
  ("Alternative tests, other ways to generate test data, other algorithms",
   [("Exact integer addition",
     property qcIntegerAddition),
    ("Integer multiplication 1",
     property $ qcIntegerMultiplication multiplyAltFS),
    ("Integer multiplication 2",
     property $ qcIntegerMultiplication multiplyAltIE),
    ("Integer multiplication 3",
     property $ qcIntegerMultiplication multiplyAltAL),
    ("Integer multiplication 4",
     property $ qcIntegerMultiplication multiplyAltAS),
    ("ArrayLookup versus ArrayState",
     property $ qcArrayLookupVersusArrayState)])

miscUnitTest :: TestBatch
miscUnitTest = 
  ("Miscellaneous unit tests",
   [("Scalar multiplication",
     property qcScalar),
    ("Multiplication self terms",
     property qcSelf),
    ("Normalize",
     property qcNormalize),
    ("Kernel chain",
     property qcChain)])

coreTest = quickSuite $
  [testBasicTernary, miscUnitTest]
  
blackBoxTest = quickSuite $ 
  testTriad ++
  [testAddition,
   testMultiplication,
   alternativeTests]

fastTest = quickCheck $
  property $ qcIntegerMultiplication multiplyAltAL


qcNormalize :: Int -> FiniteExact -> Bool
qcNormalize n x = normalizeFiniteExact depth z == z
  where
    depth = abs n `mod` 10
    as = [P1] ++ replicate (2*depth) M2
    y = unsafeFinite (Exact as $ fromIntegral depth)
    z = x * y
