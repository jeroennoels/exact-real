module Ternary.Core.Digit (
  T1(..), T2(..), T4(..),
  allT2, allT2T2, toT2, fromT1, fromT2, fromT4,
  negateT2, addT1, addT2, multiplyT2,
  coerceT1, embedT1, carry) where

import Ternary.Util.Misc (cross)

-- We want all digit operations to be type safe.  The following type
-- corresponds to the redundant ternary digit range [-2..2].  Here the
-- initial letters T/M/P abbreviate the words ternary, minus and plus.
-- The semantic mapping onto [-2..2] is defined below, but I hope it
-- is already obvious from the names.

data T2 = M2 | M1 | O0 | P1 | P2 deriving (Show, Eq, Ord, Enum)

allT2 = [M2, M1, O0, P1, P2]

allT2T2 :: [(T2,T2)]
allT2T2 = allT2 `cross` allT2

-- We want to add and multiply ternary digits from the above range.
-- Therefore we need a bigger range [-4..4] to hold intermediate
-- results.  The letter 'a' in the middle refers to addition:
-- semantically you can think of it as T2 + T2 = T4.

data T4 = Ma4 | Ma3 | Ma2 | Ma1 | Oa0 | Pa1 | Pa2 | Pa3 | Pa4
        deriving (Show, Eq)

-- Finally we also define a smaller range [-1..1].  For some
-- expressions to be T2, we need sub-expressions to be T1.

data T1 = M | O | P deriving (Show)

-- Negation is trivial because our digit range is symmetric.
negateT2 :: T2 -> T2
negateT2 M2 = P2
negateT2 M1 = P1
negateT2 O0 = O0
negateT2 P1 = M1
negateT2 P2 = M2

-- Here come the obvious semantic mappings:

fromT1 :: Num a => T1 -> a
fromT1 M = -1
fromT1 O = 0
fromT1 P = 1

fromT2 :: Num a => T2 -> a
fromT2 M2 = -2
fromT2 M1 = -1
fromT2 O0 = 0
fromT2 P1 = 1
fromT2 P2 = 2

toT2 :: Integral a => a -> T2
toT2 2 = P2
toT2 1 = P1
toT2 0 = O0
toT2 (-1) = M1
toT2 (-2) = M2

fromT4 :: Num a => T4 -> a
fromT4 Ma4 = -4
fromT4 Ma3 = -3
fromT4 Ma2 = -2
fromT4 Ma1 = -1
fromT4 Oa0 = 0
fromT4 Pa1 = 1
fromT4 Pa2 = 2
fromT4 Pa3 = 3
fromT4 Pa4 = 4

toT4 :: Integral a => a -> T4
toT4 (-4) = Ma4
toT4 (-3) = Ma3
toT4 (-2) = Ma2
toT4 (-1) = Ma1
toT4 0 = Oa0
toT4 1 = Pa1
toT4 2 = Pa2
toT4 3 = Pa3
toT4 4 = Pa4

-- Now we define digit addition and multiplication.  To keep things
-- simple, we make a round-trip to the integers.  This is not very
-- efficient, but good enough for now.

addT2 :: T2 -> T2 -> T4
addT2 a b = toT4 (fromT2 a + fromT2 b :: Int)

addT1 :: T1 -> T1 -> T2
addT1 a b = toT2 (fromT1 a + fromT1 b :: Int)

multiplyT2 :: T2 -> T2 -> T4
multiplyT2 a b = toT4 (fromT2 a * fromT2 b :: Int)

-- Sometimes, we want to view T1 as a subset of T2.

embedT1 :: T1 -> T2
embedT1 M = M1
embedT1 O = O0
embedT1 P = P1

coerceT1 :: T2 -> T1
coerceT1 M1 = M
coerceT1 O0 = O
coerceT1 P1 = P
coerceT1 x = error $ "coerceT1 applied to " ++ show x 

-- carry x = (a,b) when x = 3a+b

carry :: T4 -> (T1,T1)
carry Pa4 = (P,P)
carry Pa3 = (P,O)
carry Pa2 = (P,M)
carry Pa1 = (O,P)
carry Oa0 = (O,O)
carry Ma1 = (O,M)
carry Ma2 = (M,P)
carry Ma3 = (M,O)
carry Ma4 = (M,M)
