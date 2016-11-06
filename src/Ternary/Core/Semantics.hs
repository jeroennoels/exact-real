module Ternary.Core.Semantics (
  toT2, fromT1, fromT2, fromT4) where
       
import Ternary.Core.Digit

toT2 :: Integral a => a -> T2
toT2 2 = P2
toT2 1 = P1
toT2 0 = O0
toT2 (-1) = M1
toT2 (-2) = M2

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
