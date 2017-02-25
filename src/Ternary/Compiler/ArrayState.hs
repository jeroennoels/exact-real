{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Ternary.Compiler.ArrayState (
  MulStateAL(MulStateAL),
  MulStateAS(MulStateAS),
  arrayLookup, arrayState) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import GHC.Arr (inRange)
import GHC.Int (Int16)

import Ternary.Core.Digit (T2(..))
import Ternary.Core.Kernel (Kernel)

-- notice the thin interface here
import Ternary.Core.Multiplication (
  TriangleParam (TriangleParam),
  MultiplicationState (kernel, initialMultiplicationState))

-- notice the thin interface here
import Ternary.Compiler.ArrayLookup (
  mixIn, splitOut, lookupArray, lookupInitial,
  -- the following we only need to assert an invariant
  initialPoints, normalPointBounds, secondPointBounds)


-- Unboxed universal applied triangle
type UUAppliedTriangle = T2 -> Int16 -> (# T2, Int16 #)

-- We want to construct an array without building an assoc list first.
-- We rely on mutability for the construction phase only.
type ArrayConstruction s = ST s (STUArray s Int Int16)


uuAppliedTriangle :: UArray Int Int16 -> UUAppliedTriangle
uuAppliedTriangle array a i =
  let !ix = mixIn a (fromIntegral i)
  in splitOut (array `unsafeAt` ix)

lookupTriangle :: (T2,T2) -> UUAppliedTriangle
lookupTriangle ab = uuAppliedTriangle $! lookupArray ab

-- We now develop two variations of the inner loop of the algorithm.
-- The first one uses lists, and is therefor easier to understand but
-- not very efficient.  The second approach uses mutable arrays for
-- efficiency.  So the list variation is a stepping stone towards the
-- array version.  We shall alternate between the two approaches, to
-- emphasize the similarity and clarify the difference.  Both methods
-- use arrays for lookup, but only the second one uses arrays to hold
-- the state of the multiplication kernel.  This explains the names
-- and their abbreviations: ArrayLookup (AL) versus ArrayState (AS).

-- Because triangle parametrization has been absorbed in the state, we
-- can now simplify Ternary.Core.Kernel (chain).  It also seems faster
-- when we avoid polymorphic types inside unboxed tuples.

chainAL :: UUAppliedTriangle -> Kernel T2 T2 [Int16]
chainAL f a (u:us) =
  let (# !b, !v #) = f a u
      (c, vs) = chainAL f b us
  in (c, v:vs)
chainAL _ a [] = (a,[])

-- The arrays going in and out are zero-indexed.  The new array has
-- one more element than the old array, and the two arrays relate to
-- each other by a shift: with the exclusion of position zero, the new
-- array is completely populated.  Thus we anticipate the "final cons"
-- that will set the initial state for the next round.
chainAS :: forall s . UUAppliedTriangle -> T2 ->
           UArray Int Int16 -> (T2, ArrayConstruction s)
chainAS f start old = (x,y)
  where
    (#x,y#) = loop 0 start new
    hi = snd (bounds old)
    new = newArray_ (0, hi+1)
    -- the inner loop needs an explicit type
    loop :: Int -> T2 -> ArrayConstruction s -> (# T2, ArrayConstruction s #)
    loop i x construction
      | i > hi = (# x, construction #)
      | otherwise = let !u = unsafeAt old i  -- assume zero-indexed array!
                        (# !b, !v #) = f x u
                        j = i+1  -- shift to the right
                    in loop j b (write j v construction)

{-# INLINE write #-}  -- important
write :: Int -> Int16 -> ArrayConstruction s -> ArrayConstruction s
write i e st = do a <- st
                  writeArray a i e
                  return a


stepInvariant :: [Int16] -> Bool
stepInvariant (u:v:vs) = elem u initialPoints &&
                         inRange secondPointBounds v &&
                         all (inRange normalPointBounds) vs
stepInvariant [u] = elem u initialPoints

checkStepInvariant :: [Int16] -> [Int16]
checkStepInvariant us
  | stepInvariant us = us
  | otherwise = error "checkStepInvariant"


stepAL :: (T2,T2) -> [Int16] -> (T2, [Int16])
stepAL ab = chainAL triangle O0 . checkStepInvariant  -- O0 instead of undefined
  where !triangle = lookupTriangle ab  -- important strictness

-- The arrays going in an out are zero-indexed.
stepAS :: (T2,T2) -> UArray Int Int16 -> (T2, ArrayConstruction s)
stepAS ab = chainAS triangle O0
  where !triangle = lookupTriangle ab


newtype MulStateAL = MulStateAL [Int16] deriving Show
newtype MulStateAS = MulStateAS (UArray Int Int16) deriving Eq


multKernelAL :: Kernel (T2,T2) T2 MulStateAL
multKernelAL ab (MulStateAL us) =
  let (out, vs) = stepAL ab us
  in (out, MulStateAL (lookupInitial ab:vs))

-- The construction of a new state array is finished by setting a new
-- initial state at the beginning of the chain.  Strictness makes a
-- noticeable difference on small multiplications, which is just as
-- important as the asymptotic behavior.

multKernelAS :: Kernel (T2,T2) T2 MulStateAS
multKernelAS ab (MulStateAS us) =
  let (!out, !construction) = stepAS ab us
      !init = lookupInitial ab
      finish = write 0 init construction
  in (out, MulStateAS (runSTUArray finish))


instance MultiplicationState MulStateAL where
  kernel = multKernelAL
  initialMultiplicationState (TriangleParam a b) =
    MulStateAL [lookupInitial (a,b)]  -- singleton list

-- The construction of a new state array is finished by setting a new
-- initial state at the beginning of the chain.

instance MultiplicationState MulStateAS where
  kernel = multKernelAS
  initialMultiplicationState (TriangleParam a b) =
    MulStateAS $ array (0,0) [(0,init)]
    where init = lookupInitial (a,b)  -- singleton array

-- algorithm selector
arrayLookup :: MulStateAL
arrayLookup = undefined

arrayState :: MulStateAS
arrayState = undefined
