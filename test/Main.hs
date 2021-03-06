module Main (main) where

import System.Environment (getArgs)
import System.TimeIt

import Ternary.TestTernary (blackBoxTest, coreTest, fastTest)
import Ternary.Performance (performanceTest, evalPerformance)
import Ternary.Exhaust (exhaustiveTest, exhaustMultiplication)
import Ternary.TestCompiler (compilerTest)
import Ternary.Examples ()
import Ternary.TestExpression (expressionTest)
import Mandelbrot.Mandelbrot

main = getArgs >>= run >> putStrLn ""

run ["a"] = allTests
run ["f"] = fastTest
run ["p"] = performanceTest
run ["w"] = whiteBoxTest
run ["e"] = exhaustiveTest
run ["m"] = mandelbrotTest
run ["exp"] = expressionTest
run _ = timeIt $ sequence_ $ map putStrLn $ toImage $ sortWalk $ sampleMandelbrot

whiteBoxTest = coreTest >> compilerTest

allTests = do
  blackBoxTest
  whiteBoxTest
  exhaustMultiplication 3
  performanceTest
  expressionTest

mandelbrotTest = print (mandelbrot 3)
