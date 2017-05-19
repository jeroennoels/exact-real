{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Ternary.Exhaust (exhaustMultiplication, exhaustiveTest) where

import Control.Monad (liftM2)
import Control.Arrow ((***))

import Ternary.Compiler.ArrayLookup (warmup)
import Ternary.QuickCheckUtil (assert)
import Ternary.Util.Misc (Binop)
import Ternary.Core.Digit
import Ternary.Core.Normalize
import Ternary.List.Exact (Exact(Exact))
import Ternary.List.FiniteExact

import Test.SmallCheck
import Test.SmallCheck.Series

instance Monad m => Serial m T1 where
  series = generate $ const [M,O,P]

instance Monad m => Serial m T2 where
  series = generate $ const allT2

nDigits :: Int -> [[T2]]
nDigits n = sequence $ replicate n allT2

toFiniteExact :: [T2] -> FiniteExact
toFiniteExact ds = unsafeFinite (Exact ds 0)

exhaust :: Int -> [FiniteExact]
exhaust = map toFiniteExact . nDigits

checkOneCombination :: Binop FiniteExact -> FiniteExact -> FiniteExact -> Bool
checkOneCombination (**) x y =
  finiteExactToTriad (x ** y) ==
  finiteExactToTriad x * finiteExactToTriad y

checkAllCombinations:: Int -> Bool
checkAllCombinations depth =
  let xs = exhaust depth
      results = liftM2 (checkOneCombination multiplyAltAS) xs xs
  in null (filter not results)

exhaustMultiplication :: Int -> IO ()
exhaustMultiplication depth = assert desc (checkAllCombinations depth)
  where desc = "\nExhaust multiplication to depth " ++ show depth

exhaustiveTest = do
  smallCheck 0 scNormalize
  warmup
  exhaustMultiplication 4

-- normalize a b = (c,d) means a+3b = c+d
scNormalize :: T2 -> T1 -> Bool
scNormalize r s = a*b > 0 || a+3*b == c+d
  where a = fromT2 r
        b = fromT1 s
        (c,d) = fromT2 *** fromT1 $ normalize r s
