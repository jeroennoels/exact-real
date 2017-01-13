{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Compiler.StateSpace (
  integerEncoding, warmup) where

import Ternary.Core.Digit
import Ternary.Core.Kernel (Kernel)
import Ternary.Core.Multiplication
import Ternary.Util (cross)

import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.Set (Set, unions, union, difference, singleton, toList)
import qualified Data.Set as Set

collectSuccess :: Ord a => Set (Maybe a) -> Set a
collectSuccess as = Set.map fromJust $ Set.delete Nothing as

-- elements that can be reached in one step

reach :: forall a b . Ord b => Set a -> [a -> Maybe b] -> Set b
reach from = unions . map range
  where range :: (a -> Maybe b) -> Set b
        range f = collectSuccess $ Set.map f from

-- elements that can be reached recursively

reachTransitively :: forall a . Ord a => [a -> Maybe a] -> Set a -> Set a
reachTransitively fs from = fst $ grow (from,from)
  where grow :: (Set a, Set a) -> (Set a, Set a)
        grow pair@(acc,previous)
          | Set.null previous = pair
          | otherwise = let next = reach previous fs `difference` acc
                        in grow (acc `union` next, next)

allInputs :: [((T2, T2), T2)]
allInputs = allT2 `cross` allT2 `cross` allT2

-- Remember: on the second step, the recursive channel of a triangle
-- only accepts input between -1 and 1.

validInputForState :: TriangleState s => ((T2, T2), T2) -> s -> Bool
validInputForState (_,r) s
  | isSecondState s = r `elem` [M1,O0,P1]
  | otherwise = True

allTransitions :: TriangleState s => Triangle s -> [s -> Maybe s]
allTransitions triangle = map f allInputs
   where f i s =
           if validInputForState i s
           then Just (snd (triangle i s))
           else Nothing

reachableStates :: TriangleParam -> Set TS
reachableStates param = reachTransitively fs (singleton initialTS)
  where fs = allTransitions (makeTriangle param)


allParams = liftM2 TriangleParam allT2 allT2

-- Apparently only 157 out of 3^5 = 243 states are reachable:
-- Set.size $ unions $ map reachableStates allParams

tag :: (Ord a, Ord b) => a -> Set b -> Set (a,b)
tag a bs = Set.map section bs where section b = (a,b)

stateBundle :: Set (TriangleParam, TS)
stateBundle = unions $ map tagStates allParams
  where tagStates param = tag param (reachableStates param)


newtype CodePoint = CodePoint Int

unwrap :: CodePoint -> Int
unwrap (CodePoint i) = i

encode :: (TriangleParam, TS) -> CodePoint
encode x = CodePoint $ Set.findIndex x stateBundle

decode :: CodePoint -> (TriangleParam, TS)
decode (CodePoint i) = Set.elemAt i stateBundle

universalTriangle :: Triangle CodePoint
universalTriangle input code = (out, encode (param, nextState))
  where (param, state) = decode code
        (out, nextState) = makeTriangle param input state


instance TriangleState CodePoint where
  initialState param = encode (param, initialState param)
  makeTriangle = const universalTriangle
  isSecondState = undefined 

-- algorithm selector
integerEncoding :: MulState CodePoint
integerEncoding = undefined

warmup :: Bool
warmup = sum (map (unwrap . encode) (toList stateBundle)) == 1898326