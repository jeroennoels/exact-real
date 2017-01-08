-- No explicit exports.
-- Typeclass instances are always exported?
module Ternary.List.ExactNum () where

import Ternary.Core.Digit (T2(..), negateT2)
import Ternary.List.Exact
import Ternary.Core.Multiplication (fineStructure)

negateDigits :: [T2] -> [T2]
negateDigits = map negateT2
  
absDigits :: [T2] -> [T2]
absDigits [] = []
absDigits (O0:as) = O0 : absDigits as
absDigits x@(P1:_) = x
absDigits x@(P2:_) = x
absDigits x = negateDigits x


instance Num Exact where
  abs = unsafeApplyToDigits absDigits
  signum = error "Exact.signum is undefined" 
  negate = unsafeApplyToDigits negateDigits
  fromInteger = integerToExact 
  (+) = addExact
  (*) = multiplyExact fineStructure
