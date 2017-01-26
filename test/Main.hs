module Main where

import Ternary.TestTernary
import Ternary.Performance
import Ternary.QuickCheckUtil

main = quickSuite suite >>
       performance
