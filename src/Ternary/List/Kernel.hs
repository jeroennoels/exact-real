module Ternary.List.Kernel where

import Ternary.Core.Kernel (Kernel)

-- intentionally undefined on finite data:

recurse :: Kernel a b s -> [a] -> s -> [b]
recurse kernel (a:as) s0 =
  let (out, s1) = kernel a s0
  in out `seq` (out : recurse kernel as s1)
