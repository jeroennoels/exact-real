{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ternary.QuickCheckUtil where

import Data.Monoid (Sum(Sum), Product(Product))
import Test.QuickCheck
import Test.QuickCheck.Checkers


flattenTests :: [TestBatch] -> [Test]
flattenTests = concat . map unbatch

quickSuite ::  [TestBatch] -> IO ()
quickSuite = sequence_ . map quickBatch

test n prop = quickCheckWithResult args prop
  where args = stdArgs { maxSuccess = n, chatty = False }


instance EqProp Rational where (=-=) = eq

instance Eq a => EqProp (Sum a) where (=-=) = eq

instance Eq a => EqProp (Product a) where (=-=) = eq
