module Main where

import Ternary.TestTernary
import Ternary.Performance
import Ternary.Exhaust
import Ternary.QuickCheckUtil

main = quickSuite suite >>
       exhaustMultiplication 4 >>
       performance
