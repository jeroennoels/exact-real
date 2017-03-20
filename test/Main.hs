module Main (main) where

import System.Environment (getArgs)

import Ternary.TestTernary (blackBoxTest, coreTest, fastTest)
import Ternary.Performance (performanceTest)
import Ternary.Exhaust (exhaustiveTest, exhaustMultiplication)
import Ternary.TestCompiler (compilerTest)
import Ternary.Examples ()
import Ternary.TestExpression (expressionTest)

main = getArgs >>= run >> putStrLn ""

run ["a"] = allTests
run ["f"] = fastTest
run ["p"] = performanceTest
run ["w"] = whiteBoxTest
run ["e"] = exhaustiveTest
run ["exp"] = expressionTest
run _ =  blackBoxTest

whiteBoxTest = coreTest >> compilerTest

allTests = blackBoxTest
           >> whiteBoxTest
           >> exhaustMultiplication 3
           >> performanceTest
           >> expressionTest
          
