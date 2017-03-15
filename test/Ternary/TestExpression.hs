module Ternary.TestExpression where

import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Control.Monad (liftM, liftM2, sequence)

import Ternary.Core.Digit
import Ternary.Arbitraries
import Ternary.Sampling.Expression

arbitraryNode :: Int -> Gen Node
arbitraryNode n = liftM2 Plus below below
  where below = choose (0,n-1)

arbitraryRefNode :: Int -> Gen (Ref,Node)
arbitraryRefNode n = return n `pairM` arbitraryNode n
  where pairM = liftM2 (,)
   
arbitraryRefNodes :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes n = sequence $ leaf : map arbitraryRefNode [1..n]
  where leaf = return (0,Id)

instance Arbitrary Expr where 
  arbitrary = arbitrarySizedNatural >>= liftM expression . arbitraryRefNodes
  
expressions = sample (arbitrary :: Gen Expr)
