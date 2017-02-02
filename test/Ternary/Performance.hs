module Ternary.Performance (performance) where

import System.TimeIt

import Ternary.Core.Digit (T2(..))
import Ternary.Util.Misc (forceElements)
import Ternary.List.Exact
import Ternary.List.ExactNum ()
import Ternary.Compiler.ArrayLookup (warmup)
import Ternary.QuickCheckUtil (randomsR, assert)

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
timeMultiplication n x y =
     force (n+2) x
  >> force (n+2) y
  >> putStr "  Fine Structure   "
  >> time multiplyAltFS
  >> putStr "  Array Lookup     "
  >> time multiplyAltAL
  where time (**) = timeIt $ force n (x ** y)

performance =
  putStrLn "\nPerformance:"
  >> assertWarm
  >> timeMultiplication 3000 (randomExact 0) (randomExact 1)

