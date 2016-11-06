{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ternary.Util where

import Data.Monoid (Sum(Sum), Product(Product))
import Test.QuickCheck
import Test.QuickCheck.Checkers


eq3 :: Eq a => a -> a -> a -> Bool
eq3 a b c = a == b && b == c

assertNonNegative :: (Ord a, Num a) => String -> a -> a
assertNonNegative context x 
  | x < 0 = error $ "negative number not allowed: " ++ context
  | otherwise = x

makeRational :: Integral a => a -> a -> Rational
makeRational a b = fromIntegral a / fromIntegral b

digitsRev :: Integral a => a -> a -> [a]
digitsRev _ 0 = []
digitsRev radix k = let (cont, last) = quotRem k radix
                    in last : digitsRev radix cont

digits :: Integral a => a -> a -> [a]
digits radix = reverse . digitsRev radix

flattenTests :: [TestBatch] -> [Test]
flattenTests = concat . map unbatch

quickSuite ::  [TestBatch] -> IO ()
quickSuite = sequence_ . map quickBatch

test n prop = quickCheckWithResult args prop
  where args = stdArgs { maxSuccess = n, chatty = False}

instance EqProp Rational where (=-=) = eq

instance Eq a => EqProp (Sum a) where (=-=) = eq

instance Eq a => EqProp (Product a) where (=-=) = eq
