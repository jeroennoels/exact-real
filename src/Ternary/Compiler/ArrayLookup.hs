module Ternary.Compiler.ArrayLookup (arrayLookup, warmup) where

import Control.Arrow ((&&&))
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.Int (Int16)

import Ternary.Util.Misc (cross, toAssoc)
import Ternary.Core.Digit
import Ternary.Core.Kernel (Kernel)
import Ternary.Compiler.StateSpace
import Ternary.Core.Multiplication (
  TriangleParam (TriangleParam),
  MultiplicationState (kernel, initialMultiplicationState))

{-# INLINE mixIn #-}
mixIn :: (T2, Int16) -> Int16
mixIn (M2, i) = i
mixIn (P2, i) = i + 1540
mixIn (M1, i) = i + 3080
mixIn (O0, i) = i + 5029
mixIn (P1, i) = i + 6978

splitIn :: Int -> (T2, CodePoint)
splitIn i
  | i < 1540 = (M2, wrapNormal i)
  | i < 3080 = (P2, wrapNormal (i-1540))
  | i < 5029 = (M1, wrap (i-3080))
  | i < 6978 = (O0, wrap (i-5029))
  | i < 8927 = (P1, wrap (i-6978))
 
mixOut :: (T2, CodePoint) -> Int16
mixOut (c,p) = unwrap p + 2048 * fromIntegral (fromEnum c)

-- quotRem is slow
{-# INLINE splitOut #-}
splitOut :: Int16 -> (T2, Int16)
splitOut i | i < 2048 = (M2, i)
splitOut i | i < 4096 = (M1, i - 2048)
splitOut i | i < 6144 = (O0, i - 4096)
splitOut i | i < 8192 = (P1, i - 6144)
splitOut i            = (P2, i - 8192)


appliedUniversalTriangle :: (T2,T2) -> Int -> Int16
appliedUniversalTriangle ab i =
  let (c,code) = splitIn i
  in mixOut $ universalTriangle (ab,c) code

-- Make an array for every applied universal triangle
toArray :: (T2,T2) -> UArray Int Int16
toArray ab = array (0,n) $ toAssoc f [0..n]
  where f :: Int -> Int16
        f = appliedUniversalTriangle ab
        n = 8926

-- we use this to hash both triangle inputs and params
hash :: (T2,T2) -> Int
hash (a,b) = 5 * fromEnum a + fromEnum b

-- top level memoization
memoTriangles :: Array Int (UArray Int Int16)
memoTriangles = array (0,24) assoc
  where assoc = map (hash &&& toArray) allT2T2

-- top level memoization
memoInitial :: UArray Int Int16
memoInitial = array (0,24) assoc
  where assoc = map (hash &&& init) allT2T2
        init = unwrap . initialCodePoint . uncurry TriangleParam

lookupArray :: (T2,T2) -> UArray Int Int16
lookupArray = unsafeAt memoTriangles . hash

lookupInitial :: (T2,T2) -> Int16
lookupInitial = unsafeAt memoInitial . hash

warmup :: Bool
warmup = sum samples < 0
  where samples = elems memoInitial ++ map (!0) (elems memoTriangles)

applyTriangle :: UArray Int Int16 -> (T2,Int16) -> (T2,Int16)
applyTriangle arr pair = splitOut $ arr `unsafeAt` fromIntegral (mixIn pair)

chain :: ((a,s) -> (a,s)) -> Kernel a a [s]
chain f a (u:us) =
  let (b,v) = f (a,u)
      (c,vs) = b `seq` chain f b us
  in (c,v:vs)
chain _ a [] = (a,[])

step' :: (T2,T2) -> [Int16] -> (T2, [Int16])
step' ab = chain (applyTriangle (lookupArray ab)) O0 -- instead of undefined

newtype MulState2 = MulState2 [Int16]

multKernel' :: Kernel (T2,T2) T2 MulState2
multKernel' ab (MulState2 us) =
  let (out, vs) = step' ab us
  in (out, MulState2 (lookupInitial ab:vs))


instance MultiplicationState MulState2 where
  kernel = multKernel'
  initialMultiplicationState (TriangleParam a b) =
    MulState2 [lookupInitial (a,b)]

-- algorithm selector
arrayLookup :: MulState2
arrayLookup = undefined
