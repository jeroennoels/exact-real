{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Compiler.StateSpace (
  integerEncoding, explore, warmup) where

import Ternary.Core.Digit
import Ternary.Core.Kernel
import Ternary.Core.Multiplication
import Ternary.Util.Misc (cross)
import Ternary.Util.TransitiveClosure (reachTransitively)

import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)
import Data.List (lookup)
import GHC.Int (Int16)
import Data.Set (Set, unions, singleton, toList)
import qualified Data.Set as Set


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
allReachableStates :: Set TS
allReachableStates = unions $ map reachableStates allParams

-- And 75 of them are special:
allSecondStates :: Set TS
allSecondStates = Set.filter isSecondState allReachableStates


tag :: (Ord a, Ord b) => a -> Set b -> Set (a,b)
tag a bs = Set.map section bs where section b = (a,b)

stateBundle :: Set (TriangleParam, TS)
stateBundle = unions $ map tagStates allParams
  where tagStates param = tag param (reachableStates param)

newtype CodePoint = CodePoint Int16


unwrap :: Integral a => CodePoint -> a
unwrap (CodePoint i) = fromIntegral i

encode :: (TriangleParam, TS) -> CodePoint
encode x = CodePoint $ fromIntegral (Set.findIndex x stateBundle)

decode :: CodePoint -> (TriangleParam, TS)
decode (CodePoint i) = Set.elemAt (fromIntegral i) stateBundle

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


-- pointer arithmetic

{-# INLINE combine #-}
combine :: (T2, CodePoint) -> Int
combine (c, CodePoint p) = 5 * fromIntegral p + fromEnum c

combine' :: (T2, CodePoint) -> Int16
combine' (c, CodePoint p) = fromIntegral p + 2048 * fromIntegral (fromEnum c)

-- quotRem is slow!
{-# INLINE factor #-}
factor :: Int16 -> (T2, CodePoint)
factor i | i < 2048 = (M2, CodePoint $ i)
factor i | i < 4096 = (M1, CodePoint $ i - 2048)
factor i | i < 6144 = (O0, CodePoint $ i - 4096)
factor i | i < 8192 = (P1, CodePoint $ i - 6144)
factor i            = (P2, CodePoint $ i - 8192)

factor' :: Int -> (T2, CodePoint)
factor' i = (toEnum r, CodePoint (fromIntegral q))
  where (q,r) = quotRem i 5


appliedUniversalTriangle :: (T2,T2) -> Int -> Int16
appliedUniversalTriangle ab i =
  let (c,code) = factor' i
      (_,state) = decode code
  in if isSecondState state && c `elem` [M2,P2]
     then -1  -- this is ugly
     else combine' (universalTriangle (ab,c) code)

toAssoc :: (a -> b) -> [a] -> [(a,b)]
toAssoc f = map graph
  where graph a = (a, f a)

appliedUniversalTriangleAssoc :: (T2,T2) -> [(Int,Int16)]        
appliedUniversalTriangleAssoc ab = toAssoc (appliedUniversalTriangle ab) [0..n]
  where n = 5 * Set.size stateBundle - 1

applyTriangle' :: (T2,T2) -> (T2,CodePoint) -> (T2,CodePoint)
applyTriangle' ab pair = factor $ memo ab `unsafeAt` combine pair

chain' :: ((a,s) -> (a,s)) -> Kernel a a [s]
chain' f a (u:us) =
  let (b,v) = f (a,u)
      (c,vs) = b `seq` chain' f b us
  in (c,v:vs)
chain' _ a [] = (a,[])

step' :: (T2,T2) -> [CodePoint] -> (T2, [CodePoint])
step' ab = chain' (applyTriangle' ab) O0 -- unsafeIgnoreInput no longer works 

newtype MulState2 = MulState2 [CodePoint]

multKernel' :: Kernel (T2,T2) T2 MulState2
multKernel' ab (MulState2 us) =
  let (out, vs) = step' ab us
      p = uncurry TriangleParam $ ab
  in (out, MulState2 (initialState p:vs))


instance MultiplicationState MulState2 where
  kernel = multKernel'
  initialMultiplicationState p = MulState2 [initialState p]

-- algorithm selector
explore :: MulState2
explore = undefined

toArray :: (T2,T2) -> UArray Int Int16
toArray ab = array (0, length list - 1) list
  where list = appliedUniversalTriangleAssoc ab

arrays = toAssoc toArray (allT2 `cross` allT2)

memo ab = fromJust $ lookup ab arrays

warmup :: Bool
warmup = sum (map ((!0) . snd) arrays) == -19043
