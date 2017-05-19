module Ternary.Core.Normalize (normalize) where

import Ternary.Core.Kernel (Kernel)
import Ternary.Core.Digit (T1(..), T2(..))

-- normalize a b = (c,d) means a+3b = c+d, or bottom when a and b are
-- both positive or both negative.

normalize :: Kernel T2 T2 T1
normalize a O = (a,O)
normalize M2 P = (P1,O)
normalize M1 P = (P2,O)
normalize O0 P = (P2,P)
normalize P2 M = (M1,O)
normalize P1 M = (M2,O)
normalize O0 M = (M2,M)
normalize _ _ = error "cannot normalize"
