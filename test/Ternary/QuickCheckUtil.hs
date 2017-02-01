{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ternary.QuickCheckUtil where

import System.TimeIt
import System.Random (Random, StdGen, randomR, mkStdGen)
import Data.Monoid (Sum(Sum), Product(Product))
import Test.QuickCheck
import Test.QuickCheck.Checkers

assert :: String -> Bool -> IO ()
assert description test = timeIt $ putStr text
  where text = "  " ++ description ++ " => " ++ result ++ ", "
        result = if test then "OK" else "FAILED"


randomsR :: Random a => Int -> (a,a) -> [a]
randomsR seed range = go (mkStdGen seed)
  where go gen = let (a,next) = randomR range gen
                 in a : go next

flattenTests :: [TestBatch] -> [Test]
flattenTests = concat . map unbatch

quickSuite ::  [TestBatch] -> IO ()
quickSuite = sequence_ . map quickBatch

test n prop = quickCheckWithResult args prop
  where args = stdArgs { maxSuccess = n, chatty = False }


instance EqProp Rational where (=-=) = eq

instance Eq a => EqProp (Sum a) where (=-=) = eq

instance Eq a => EqProp (Product a) where (=-=) = eq
