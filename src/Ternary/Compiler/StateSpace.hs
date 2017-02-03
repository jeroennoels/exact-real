module Ternary.Compiler.StateSpace (
  CodePoint, initialCodePoint,
  universalTriangle, integerEncoding,
  unwrap, wrap, wrapNormal) where

import Control.Monad (liftM2)
import Data.Set (Set, unions, singleton, partition, findIndex)
import qualified Data.Set as Set

import Ternary.Core.Digit
import Ternary.Core.Kernel
import Ternary.Core.Multiplication
import Ternary.Util.Misc (cross)
import Ternary.Util.SetUtils (reachTransitively, assertSize, tag)

-- Remember: on the second step, the recursive channel of a triangle
-- only accepts input between -1 and 1.
validInputForState :: TriangleState s => ((T2, T2), T2) -> s -> Bool
validInputForState (_,r) s
  | isSecondState s = r `elem` [M1,O0,P1]
  | otherwise = True

-- For some inputs, the corresponding transition function is partial.
allTransitions :: TriangleState s => Triangle s -> [s -> Maybe s]
allTransitions triangle = map nextState allInputs
  where allInputs = allT2T2 `cross` allT2
        nextState i s =
          if validInputForState i s
          then Just $ snd (triangle i s)
          else Nothing
         
reachableStates :: (Ord s, TriangleState s) => TriangleParam -> Set s
reachableStates param = reachTransitively fs (singleton $ initialState param)
  where fs = allTransitions (makeTriangle param)

allParams = liftM2 TriangleParam allT2 allT2

-- Apparently only 157 out of 3^5 = 243 states are reachable:
allReachableStates :: Set TS
allReachableStates = unions $ map reachableStates allParams

-- And 75 of them are special:
allSecondStates :: Set TS
allSecondStates = Set.filter isSecondState allReachableStates

-- For every triangle parametrization, we have a set of reachable
-- states.  Here we make a disjoint union of all these sets.
type Bundle = Set (TriangleParam, TS)

stateBundle :: Bundle
stateBundle = unions $ map tagStates allParams
  where tagStates param = tag param (reachableStates param)

bundlePair :: (Bundle, Bundle)
bundlePair = partition pred stateBundle
  where pred (_,s) = isSecondState s 

secondStateBundle, normalStateBundle :: Bundle
secondStateBundle = assertSize (fst bundlePair) 409
normalStateBundle = assertSize (snd bundlePair) 1540

data CodePoint = Normal Int | Second Int deriving Eq

rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lo hi x
  | lo <= x && x <= hi = x
  | otherwise = error "rangeCheck"

wrapNormal, wrapSecond :: Int -> CodePoint
wrapNormal = Normal . rangeCheck 0 1539
wrapSecond = Second . rangeCheck 1540 1948
  
wrap :: Int -> CodePoint
wrap i = if i < 1540 then wrapNormal i else wrapSecond i

unwrap :: Integral i => CodePoint -> i
unwrap (Normal i) = fromIntegral i
unwrap (Second i) = fromIntegral i

encode :: (TriangleParam, TS) -> CodePoint
encode x@(_,s) = 
  if isSecondState s 
  then wrapSecond $ findIndex x secondStateBundle + 1540
  else wrapNormal $ findIndex x normalStateBundle

decode :: CodePoint -> (TriangleParam, TS)
decode (Normal i) = Set.elemAt i normalStateBundle
decode (Second i) = Set.elemAt (i-1540) secondStateBundle

-- The following triangle is "universal" in the sense that it includes
-- a triangle for every parametrization, because a TriangleParam is
-- embedded in each CodePoint.  Therefor the initial state determines
-- the parametrization.

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

initialCodePoint :: TriangleParam -> CodePoint
initialCodePoint = initialState
