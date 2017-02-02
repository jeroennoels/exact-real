module Ternary.Compiler.ArrayLookup (
  splitIn, splitOut, mixIn, mixOut,
  arrayLookup, warmup) where

import Control.Arrow ((&&&))
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.Int (Int16)

import Ternary.Util.Misc (cross, toAssoc, forceElements)
import Ternary.Core.Digit
import Ternary.Core.Kernel (Kernel)
import Ternary.Compiler.StateSpace
import Ternary.Core.Multiplication (
  TriangleParam (TriangleParam),
  MultiplicationState (kernel, initialMultiplicationState))

-- The functions splitIn and mixOut are only used by the compiler,
-- whereas mixIn and splitOut belong to the run-time.  Therefor only
-- the latter must really be optimized for efficiency.

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

-- CodePoint wraps integers in the range [0..1948]              
mixOut :: (T2, CodePoint) -> Int16
mixOut (a,code) = unwrap code + 2048 * fromIntegral (fromEnum a)

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

-- make an array for every applied universal triangle
toArray :: (T2,T2) -> UArray Int Int16
toArray ab = array (0,n) $ toAssoc f [0..n]
  where f :: Int -> Int16
        f = appliedUniversalTriangle ab
        n = 8926

-- we use this to hash both triangle inputs and params
hash :: (T2,T2) -> Int
hash (a,b) = 5 * fromEnum a + fromEnum b

-- consistency check on the hash function
hashRange = if map hash allT2T2 == [0..24]
            then (0,24) else error "hashRange"

-- top level memoization
memoTriangles :: Array Int (UArray Int Int16)
memoTriangles = array hashRange assoc
  where assoc = map (hash &&& toArray) allT2T2

-- top level memoization
memoInitial :: UArray Int Int16
memoInitial = array hashRange assoc
  where assoc = map (hash &&& init) allT2T2
        init = unwrap . initialCodePoint . uncurry TriangleParam

lookupArray :: (T2,T2) -> UArray Int Int16
lookupArray = unsafeAt memoTriangles . hash

lookupInitial :: (T2,T2) -> Int16
lookupInitial = unsafeAt memoInitial . hash

warmup :: IO ()
warmup = return $! forceElements samples
  where samples :: [Int16] 
        samples = elems memoInitial ++ map (!0) (elems memoTriangles)


appliedTriangle :: UArray Int Int16 -> (T2,Int16) -> (T2,Int16)
appliedTriangle array = splitOut . unsafeAt array . fromIntegral . mixIn

-- Because triangle parametrization has been absorbed in the state, we
-- can now simplify Ternary.Core.Kernel.chain as follows:

chain :: ((a,s) -> (a,s)) -> Kernel a a [s]
chain f a (u:us) =
  let (b,v) = f (a,u)
      (c,vs) = b `seq` chain f b us
  in (c,v:vs)
chain _ a [] = (a,[])

step :: (T2,T2) -> [Int16] -> (T2, [Int16])
step ab = chain (appliedTriangle (lookupArray ab)) O0 -- instead of undefined

newtype MulState2 = MulState2 [Int16]

multKernel :: Kernel (T2,T2) T2 MulState2
multKernel ab (MulState2 us) =
  let (out, vs) = step ab us
  in (out, MulState2 (lookupInitial ab:vs))


instance MultiplicationState MulState2 where
  kernel = multKernel
  initialMultiplicationState (TriangleParam a b) =
    MulState2 [lookupInitial (a,b)]

-- algorithm selector
arrayLookup :: MulState2
arrayLookup = undefined
