module Ternary.Util.Misc where

import Control.Monad (liftM2)
import Data.Array.IArray (IArray, array)


type Binop a = a -> a -> a

both :: Maybe a -> Maybe b -> Maybe (a,b)
both (Just a) (Just b) = Just (a,b)
both _ _ = Nothing

{-# INLINE rangeCheck #-}
rangeCheck :: Ord a => a -> a -> a -> a
rangeCheck lo hi x
  | lo <= x && x <= hi = x
  | otherwise = error "rangeCheck"

-- Force all the elements of a list.  Shallow, not deep.
forceElements :: [a] -> ()
forceElements = foldr seq ()

forceElementsIO :: [a] -> IO ()
forceElementsIO = (return $!) . forceElements 

toAssoc :: (a -> b) -> [a] -> [(a,b)]
toAssoc f = map graph
  where graph a = (a, f a)

zeroIndexedArray :: IArray a e => [e] -> a Int e
zeroIndexedArray elems = array (0,n) (zip [0..n] elems)
  where n = length elems - 1

cross :: [a] -> [b] -> [(a,b)]
cross = liftM2 (,)

eq3 :: Eq a => a -> a -> a -> Bool
eq3 a b c = a == b && b == c

assertNonNegative :: (Ord a, Num a) => String -> a -> a
assertNonNegative context x
  | x < 0 = error $ "Negative number not allowed: " ++ context
  | otherwise = x

makeRational :: Integral a => a -> a -> Rational
makeRational a b = fromIntegral a / fromIntegral b

digitsRev :: Integral a => a -> a -> [a]
digitsRev _ 0 = []
digitsRev radix k = let (cont, last) = quotRem k radix
                    in last : digitsRev radix cont

digits :: Integral a => a -> a -> [a]
digits radix = reverse . digitsRev radix

strictlyIncreasing :: Ord a => [a] -> Bool
strictlyIncreasing list = and $ zipWith (<) list (tail list)
