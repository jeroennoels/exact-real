module Ternary.Util.Misc where

import Control.Monad (liftM2)

type Binop a = a -> a -> a

toAssoc :: (a -> b) -> [a] -> [(a,b)]
toAssoc f = map graph
  where graph a = (a, f a)

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
