{-# LANGUAGE ScopedTypeVariables #-}
module Ternary.Compiler.StateSpace where

import Ternary.Core.Kernel
import Ternary.Core.Digit
import Ternary.Core.Multiplication

import Ternary.Util (cross)

import Control.Monad (liftM2)
import Data.Set (Set, unions, union, difference, singleton)
import qualified Data.Set as Set

-- elements that can be reached in one step

reach :: forall a b . Ord b => Set a -> [a -> b] -> Set b
reach from = unions . map range
   where range :: (a -> b) -> Set b
         range f = Set.map f from

-- elements that can be reached recursively

reachTransitively :: forall a . Ord a => [a -> a] -> Set a -> Set a
reachTransitively fs from = fst $ grow (from,from)
  where grow :: (Set a, Set a) -> (Set a, Set a)
        grow pair@(acc,previous)
          | Set.null previous = pair 
          | otherwise = let next = reach previous fs `difference` acc
                        in grow (acc `union` next, next)


-- We gather all valid inputs for a triangle.  Remember the recursion
-- channel allows only inputs between -1 and 1.

allInputs :: [((T2, T2), T2)]
allInputs = allT2 `cross` allT2 `cross` [M1,O0,P1]

allTransitions :: Triangle s -> [s -> s]
allTransitions triangle = map f allInputs
   where f i s = snd $ triangle i s

reachableStates :: TriangleParam -> Set TS
reachableStates param = reachTransitively fs (singleton initialTS)
  where fs = allTransitions (makeTriangle param)

-- Apparently only 157 out of 3^5 = 243 states are reachable
        
allReachable = unions $ map reachableStates allParams
  where allParams = liftM2 TriangleParam allT2 allT2
