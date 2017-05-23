module Ternary.Core.Normalize where

import Ternary.Core.Kernel (Kernel, iterateKernel)
import Ternary.Core.Digit (T1(..), T2(..), coerceT1)

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

initNormalize :: [T2] -> [T1]
initNormalize = initNormalizeRev . reverse

initNormalizeRev :: [T2] -> [T1]
initNormalizeRev [] = []
initNormalizeRev (a:as) =
  let kern = iterateKernel normalize (length as)
      (c,bs) = kern a (initNormalizeRev as)
  in bs ++ [coerceT1 c]
