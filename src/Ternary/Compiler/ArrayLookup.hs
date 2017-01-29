module Ternary.Compiler.ArrayLookup (arrayLookup, warmup) where

import Control.Arrow (second)
import Data.Maybe (fromJust)
import Data.Array.Unboxed
import Data.Array.Base (unsafeAt)
import Data.List (lookup)
import GHC.Int (Int16)

import Ternary.Util.Misc (cross)
import Ternary.Core.Digit
import Ternary.Core.Kernel
import Ternary.Core.Multiplication
import Ternary.Compiler.StateSpace

{-# INLINE mixIn #-}
mixIn :: (T2, CodePoint) -> Int
mixIn (M2, Normal i) = fromIntegral i
mixIn (P2, Normal i) = fromIntegral i + 1540
mixIn (M1, point) = unwrap point + 3080
mixIn (O0, point) = unwrap point + 5029
mixIn (P1, point) = unwrap point + 6978


splitIn :: Int -> (T2, CodePoint)
splitIn i
  | i < 1540 = (M2, wrapNormal i)
  | i < 3080 = (P2, wrapNormal (i-1540))
  | i < 5029 = (M1, wrap (i-3080))
  | i < 6978 = (O0, wrap (i-5029))
  | i < 8927 = (P1, wrap (i-6978))
 
mixOut :: (T2, CodePoint) -> Int16
mixOut (c,p) = unwrap p + 2048 * fromIntegral (fromEnum c)

{-# INLINE split #-}
-- quotRem is slow!
split :: Int16 -> (T2, Int16)
split i | i < 2048 = (M2, i)
split i | i < 4096 = (M1, i - 2048)
split i | i < 6144 = (O0, i - 4096)
split i | i < 8192 = (P1, i - 6144)
split i            = (P2, i - 8192)

{-# INLINE splitOut #-}
splitOut :: Int16 -> (T2, CodePoint)
splitOut = second wrap . split


appliedUniversalTriangle :: (T2,T2) -> Int -> Int16
appliedUniversalTriangle ab i =
  let (c,code) = splitIn i
  in mixOut $ universalTriangle (ab,c) code


toAssoc :: (a -> b) -> [a] -> [(a,b)]
toAssoc f = map graph
  where graph a = (a, f a)

appliedUniversalTriangleAssoc :: (T2,T2) -> [(Int,Int16)]        
appliedUniversalTriangleAssoc ab = toAssoc (appliedUniversalTriangle ab) [0..n]
  where n = 2 * 1540 + 3 * 1949  - 1

applyTriangle' :: (T2,T2) -> (T2,CodePoint) -> (T2,CodePoint)
applyTriangle' ab pair = splitOut $ memo ab `unsafeAt` mixIn pair

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
arrayLookup :: MulState2
arrayLookup = undefined

toArray :: (T2,T2) -> UArray Int Int16
toArray ab = array (0, length list - 1) list
  where list = appliedUniversalTriangleAssoc ab

arrays = toAssoc toArray (allT2 `cross` allT2)

memo ab = fromJust $ lookup ab arrays

warmup :: Bool
warmup = sum (map ((!0) . snd) arrays) == 18280
