module Main (main) where

import System.Environment (getArgs)

import Ternary.TestTernary (suite, fast)
import Ternary.Performance (performance)
import Ternary.Exhaust (exhaustMultiplication)
import Ternary.TestCompiler (testCompiler)
import Ternary.QuickCheckUtil (quickSuite)

main = getArgs >>= run >> putStrLn ""

run ["p"] = performance
run ["f"] = fast
run _ = full

full = quickSuite suite >>
       performance >>
       exhaustMultiplication 3 >>
       testCompiler
