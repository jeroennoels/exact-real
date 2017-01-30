module Ternary.Performance where

import System.TimeIt

import Ternary.Core.Digit (T2(..))
import Ternary.List.Exact
import Ternary.List.ExactNum
import Ternary.Compiler.ArrayLookup (warmup)
import Ternary.QuickCheckUtil


randomT2s :: Int -> [T2]              
randomT2s seed = map toEnum (randomsR seed (0,4))

assertWarm :: IO ()
assertWarm = timeIt $ putStr (text ++ "      ")
  where text = "  Warmup: " ++ if warmup then "done" else "error"

force :: Int -> Exact -> IO ()
force n x = streamDigits x !! n `seq` return ()

timeMultiplication :: Int -> Exact -> Exact -> IO ()
timeMultiplication n x y = 
  force (n+2) x
  >> force (n+2) y
  >> putStr "  Fine Structure    "
  >> time multiplyAltFS
  >> putStr "  Array Lookup      "  
  >> time multiplyAltAL
  where time (**) = timeIt $ force n (x ** y)


a,b :: Exact
a = Exact (randomT2s 0) 0
b = Exact (randomT2s 1) 0

performance =
  putStrLn "\nPerformance:"
  >> assertWarm
  >> timeMultiplication 2000 a b

