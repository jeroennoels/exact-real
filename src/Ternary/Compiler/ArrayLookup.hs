{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Ternary.Compiler.ArrayLookup (
  splitIn, splitOut, mixIn, mixOut,
  warmup, lookupArray, lookupInitial) where

import Control.Arrow ((&&&))
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.Int (Int16)

import Ternary.Util.Misc (cross, toAssoc, forceElements)
import Ternary.Core.Digit (T2(..), allT2T2)
import Ternary.Core.Kernel (Kernel)
import Ternary.Compiler.StateSpace
import Ternary.Core.Multiplication (TriangleParam (TriangleParam))

-- The functions splitIn and mixOut are only used by the compiler,
-- whereas mixIn and splitOut belong to the run-time.  Therefor only
-- the latter must really be optimized for efficiency.  So only for
-- these functions, we use low-level tricks such as unboxed tuples.

{-# INLINE mixIn #-}
{-# INLINE mixOut #-}
{-# INLINE splitIn #-}
{-# INLINE splitOut #-}

-- runtime computation
mixIn :: T2 -> Int -> Int
mixIn M2 i = i
mixIn P2 i = i + 1540
mixIn M1 i = i + 3080
mixIn O0 i = i + 5029
mixIn P1 i = i + 6978

-- We explain and verify those hard-coded numbers.  Remember that code
-- points are represented by two adjacent integer ranges: respectively
-- [0..1539] and [1540..1948] for normal and second step states.

verifyMixIn (lo,hi) =
  mixIn M2 0 == lo &&
  mixIn P2 0 == mixIn M2 0 + 1540 &&    -- M2: only normal states
  mixIn M1 0 == mixIn P2 0 + 1540 &&    -- P2: only normal states
  mixIn O0 0 == mixIn M1 0 + 1949 &&    -- M1: all states
  mixIn P1 0 == mixIn O0 0 + 1949 &&    -- O0: all states
  mixIn P1 1948 == hi                   -- P1: all states

-- compile-time verification
verify :: (Int,Int) -> (Int,Int)
verify range = if verifyMixIn range then range
               else error "ArrayLookup (verify)"


-- compile-time computation
splitIn :: Int -> (T2, CodePoint)
splitIn i
  | i < 0 = error "splitIn"
  | i < 1540 = (M2, wrapNormal i)
  | i < 3080 = (P2, wrapNormal (i-1540))
  | i < 5029 = (M1, wrap (i-3080))
  | i < 6978 = (O0, wrap (i-5029))
  | i < 8927 = (P1, wrap (i-6978))
  | otherwise = error "splitIn"

-- CodePoint wraps integers in the range [0..1948]
-- This is a compile-time computation.
mixOut :: (T2, CodePoint) -> Int16
mixOut (a,code) = unwrap code + 2048 * fromIntegral (fromEnum a)

-- runtime computation, quotRem is slow
splitOut :: Int16 -> (# T2, Int16 #)
splitOut i | i < 2048 = (# M2, i #)
splitOut i | i < 4096 = (# M1, i-2048 #)
splitOut i | i < 6144 = (# O0, i-4096 #)
splitOut i | i < 8192 = (# P1, i-6144 #)
splitOut i            = (# P2, i-8192 #)


appliedUniversalTriangle :: (T2,T2) -> Int -> Int16
appliedUniversalTriangle ab i =
  let (c,code) = splitIn i
  in mixOut $ universalTriangle (ab,c) code

-- Make a zero-indexed array for every applied universal triangle.
toArray :: (T2,T2) -> UArray Int Int16
toArray ab = array bounds $ toAssoc f domain
  where f :: Int -> Int16
        f = appliedUniversalTriangle ab
        bounds = verify (0,8926)
        domain = uncurry enumFromTo bounds

-- We use this to hash both triangle inputs and params.
hash :: (T2,T2) -> Int
hash (a,b) = 5 * fromEnum a + fromEnum b

-- Consistency check on the hash function.  The range is zero-indexed.
hashRange = if map hash allT2T2 == [0..24]
            then (0,24)
            else error "ArrayLookup (hashRange)"

-- top level memoization, using nothing but zero-indexed arrays
memoTriangles :: Array Int (UArray Int Int16)
memoTriangles = array hashRange assoc
  where assoc = map (hash &&& toArray) allT2T2

-- top level memoization, using a zero-indexed array
memoInitial :: UArray Int Int16
memoInitial = array hashRange assoc
  where assoc = map (hash &&& init) allT2T2
        init = unwrap . initialCodePoint . uncurry TriangleParam

{-# INLINE lookupInitial #-}
{-# INLINE lookupArray #-}

-- obtain the correct precomputed array for a given input
lookupArray :: (T2,T2) -> UArray Int Int16
lookupArray = unsafeAt memoTriangles . hash

lookupInitial :: (T2,T2) -> Int16
lookupInitial = unsafeAt memoInitial . hash

warmup :: IO ()
warmup = return $! forceElements samples
  where samples :: [Int16]
        samples = elems memoInitial ++ map (!0) (elems memoTriangles)
