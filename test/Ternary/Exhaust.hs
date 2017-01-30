module Ternary.Exhaust (exhaustMultiplication) where

import System.TimeIt
import Control.Monad (liftM2)

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
exhaustMultiplication depth = timeIt $ putStr ("\n" ++ text ++ ", ")
  where
    text = "Exhaust multiplication to depth " ++ show depth ++ " => " ++ result
    test = checkAllCombinations depth
    result = if test then "OK" else "FAILED!"
