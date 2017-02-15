{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Ternary.Compiler.ArrayState (arrayLookup, arrayState) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import GHC.Int (Int16)

import Ternary.Core.Digit (T2(..))
import Ternary.Core.Kernel (Kernel)

-- notice the thin interface here
import Ternary.Core.Multiplication (
  TriangleParam (TriangleParam),
  MultiplicationState (kernel, initialMultiplicationState))

-- notice the thin interface here
import Ternary.Compiler.ArrayLookup (
  mixIn, splitOut, lookupArray, lookupInitial)


-- unboxed universal applied triangle
type UUAppliedTriangle = T2 -> Int16 -> (# T2, Int16 #)

type ArrayConstruction s = ST s (STUArray s Int Int16)


uuAppliedTriangle :: UArray Int Int16 -> UUAppliedTriangle
uuAppliedTriangle array a i =
  let !ix = mixIn a (fromIntegral i)
  in splitOut (array `unsafeAt` ix)

lookupTriangle :: (T2,T2) -> UUAppliedTriangle
lookupTriangle = uuAppliedTriangle . lookupArray


-- Because triangle parametrization has been absorbed in the state, we
-- can now simplify Ternary.Core.Kernel (chain).  It also seems faster
-- when we avoid polymorphic types inside unboxed tuples.

chainAL :: UUAppliedTriangle -> Kernel T2 T2 [Int16]
chainAL f a (u:us) =
  let (# !b, !v #) = f a u
      (c, vs) = chainAL f b us
  in (c, v:vs)
chainAL _ a [] = (a,[])


chainAS :: forall s . UUAppliedTriangle -> T2 ->
           UArray Int Int16 -> (T2, ArrayConstruction s)
chainAS f start old = (x,y)
  where
    (#x,y#) = loop lo start new
    (lo,hi) = bounds old
    new = newArray (lo, hi+1) (-1)
    loop :: Int -> T2 -> ArrayConstruction s -> (# T2, ArrayConstruction s #)
    loop i x construction
      | i > hi = (# x, construction #)
      | otherwise = let !u = unsafeAt old i
                        (# !b, !v #) = f x u
                        j = i+1  -- shift to the right
                    in loop j b (write j v construction)


write :: Int -> Int16 -> ArrayConstruction s -> ArrayConstruction s
write i e st = do a <- st 
                  writeArray a i e
                  return a


stepAL :: (T2,T2) -> [Int16] -> (T2, [Int16])
stepAL ab = chainAL triangle O0        -- instead of undefined
  where !triangle = lookupTriangle ab  -- important strictness

stepAS :: (T2,T2) -> UArray Int Int16 -> (T2, ArrayConstruction s)
stepAS ab = chainAS triangle O0        -- instead of undefined
  where !triangle = lookupTriangle ab  -- important strictness


newtype MulStateAL = MulStateAL [Int16]
newtype MulStateAS = MulStateAS (UArray Int Int16)


multKernelAL :: Kernel (T2,T2) T2 MulStateAL
multKernelAL ab (MulStateAL us) =
  let (out, vs) = stepAL ab us
  in (out, MulStateAL (lookupInitial ab:vs))

multKernelAS :: Kernel (T2,T2) T2 MulStateAS
multKernelAS ab (MulStateAS us) =
  let (out, st) = stepAS ab us
      initStart = do
        arr <- st
        writeArray arr 0 $ lookupInitial ab
        return arr
  in (out, MulStateAS (runSTUArray initStart))


instance MultiplicationState MulStateAL where
  kernel = multKernelAL
  initialMultiplicationState (TriangleParam a b) =
    MulStateAL [lookupInitial (a,b)]

instance MultiplicationState MulStateAS where
  kernel = multKernelAS
  initialMultiplicationState (TriangleParam a b) =
    MulStateAS $ array (0,0) [(0,init)]
    where init = lookupInitial (a,b)

-- algorithm selector
arrayLookup :: MulStateAL
arrayLookup = undefined

arrayState :: MulStateAS
arrayState = undefined
