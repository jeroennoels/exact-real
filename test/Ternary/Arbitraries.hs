module Ternary.Arbitraries where

import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Control.Monad (liftM2)

import Ternary.Core.Digit
import Ternary.Util.Triad (Triad, makeTriad)


instance Arbitrary T1 where
  arbitrary = elements [M, O, P]

instance Arbitrary T2 where
  arbitrary = elements allT2

instance Arbitrary T4 where
  arbitrary = elements [Ma4, Ma3, Ma2, Ma1, Oa0, Pa1, Pa2, Pa3, Pa4]

instance Arbitrary Triad where
  arbitrary = liftM2 makeTriad arbitrary exponent
    where exponent = fmap getNonNegative arbitrary
