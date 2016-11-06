module Ternary.Core.Digit (
  T1(..), T2(..), T4(..), 
  negateT2, addT1, addT2, multiplyT2,
  coerceT1, embedT1, carry) where

-- We want all digit operations to be type safe.  The following type
-- corresponds to the redundant ternary digit range [-2..2].  The
-- initial letters T/M/P abbreviate the words ternary, minus and plus.

data T2 = M2 | M1 | O0 | P1 | P2
        deriving (Show, Eq, Enum)

-- Negation is trivial because our digit range is symmetric.

negateT2 :: T2 -> T2
negateT2 M2 = P2
negateT2 M1 = P1
negateT2 O0 = O0
negateT2 P1 = M1
negateT2 P2 = M2

-- We want to add ternary digits from the above range.
-- Therefor we also need a bigger range: [-4..4]

data T4 = Ma4 | Ma3 | Ma2 | Ma1 | Oa0 | Pa1 | Pa2 | Pa3 | Pa4
        deriving (Show, Eq, Enum)

-- To keep things simple, we make a round trip to the integers using
-- the Enum methods.  This is not very efficient, but good enough for
-- now.  The conversion offsets cancel out.
-- TODO: is it a hack to rely on Enum for this?

addT2 :: T2 -> T2 -> T4
addT2 a b = toEnum (fromEnum a + fromEnum b)

-- We do the same for multiplication. We must be careful with the Enum
-- methods: the offsets do not compose nicely.

multiplyT2 :: T2 -> T2 -> T4
multiplyT2 a b = toEnum (symm a * symm b + 4)
  -- symm O0 = 0 
  where symm :: T2 -> Int
        symm x = fromEnum x - 2

-- For some expressions to be T2, we need sub-expressions to be T1.

data T1 = M | O | P deriving (Show, Enum)

addT1 :: T1 -> T1 -> T2
addT1 a b = toEnum (fromEnum a + fromEnum b)

coerceT1 :: T2 -> T1
coerceT1 M1 = M
coerceT1 O0 = O
coerceT1 P1 = P
coerceT1 x = error $ "coerceT1 applied to " ++ show x 

embedT1 :: T1 -> T2
embedT1 M = M1
embedT1 O = O0
embedT1 P = P1

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
