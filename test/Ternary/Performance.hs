module Ternary.Performance (
  performanceTest, evalPerformance) where

import System.TimeIt

import Ternary.Core.Digit (T2(..))
import Ternary.Util.Misc (forceElements, forceElementsIO)
import Ternary.List.Exact
import Ternary.List.ExactNum ()
import Ternary.Compiler.ArrayLookup (warmup)
import Ternary.Sampling.Expression
import Ternary.Sampling.Evaluation
import Ternary.QuickCheckUtil (randomsR)


randomT2s :: Int -> [T2]
randomT2s seed = map toEnum (randomsR seed (0,4))

randomExact :: Int -> Exact
randomExact seed = Exact (randomT2s seed) 0

assertWarm :: IO ()
assertWarm = putStr "  Warmup: " >> timeIt warmup

-- The time needed to construct random test samples must be excluded
-- from measurements.  On the flip side, the time to construct the
-- final result of a computation must be included.  The following
-- ensures the first n digits of an exact number are fully evaluated:
force :: Int -> Exact -> IO ()
force n = (return $!) . forceElements . take n . streamDigits

timeMultiplication :: Int -> Exact -> Exact -> IO ()
timeMultiplication n x y = do
  force (n+2) x
  force (n+2) y
  putStr "  Array Lookup  "
  time multiplyAltAL
  putStr "  Array State   "
  time multiplyAltAS
  where
    time (**) = timeIt $ force n (x ** y)

performanceTest = do
  putStrLn "\nPerformance:"
  assertWarm
  timeMultiplication 6000 (randomExact 0) (randomExact 1)


timeExpressionEval :: Expr -> [T2] -> IO ()
timeExpressionEval expr as = do
  forceElementsIO as
  len <- time (evalFinite1 expr as)
  time (take len (streamDigits $ smartEval expr binding))
  putStrLn ("Number of output digits = " ++ show len)
  where
    binding = bind (Exact as 0)
    time list = timeIt (forceElementsIO list >> return (length list))
    
evalPerformance = do
  timeExpressionEval (extreme Mins 20000) (take 5 $ randomT2s 0)
  timeExpressionEval (extreme Plus 60) (take 8000 $ randomT2s 0)

