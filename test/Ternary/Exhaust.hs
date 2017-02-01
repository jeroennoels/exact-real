module Ternary.Exhaust (exhaustMultiplication) where

import Control.Monad (liftM2)

import Ternary.QuickCheckUtil (assert)
import Ternary.Util.Misc (Binop)
import Ternary.Core.Digit
import Ternary.List.Exact (Exact(Exact))
import Ternary.List.FiniteExact


nDigits :: Int -> [[T2]]
nDigits n = sequence $ replicate n allT2

toFiniteExact :: [T2] -> FiniteExact
toFiniteExact ds = unsafeFinite (Exact ds 0)

exhaust :: Int -> [FiniteExact]
exhaust depth = map toFiniteExact (nDigits depth)

checkOneCombination :: Binop FiniteExact -> FiniteExact -> FiniteExact -> Bool
checkOneCombination (**) x y =
  finiteExactToTriad (x ** y) == finiteExactToTriad x * finiteExactToTriad y

checkAllCombinations:: Int -> Bool
checkAllCombinations depth =
  let xs = exhaust depth
      results = liftM2 (checkOneCombination multiplyAltAL) xs xs
  in null (filter not results)

exhaustMultiplication :: Int -> IO ()
exhaustMultiplication depth = assert desc (checkAllCombinations depth)
  where desc = "Exhaust multiplication to depth " ++ show depth 

