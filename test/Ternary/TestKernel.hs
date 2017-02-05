module Ternary.TestKernel (
  qcChain, testChainForSpaceLeak) where

import Ternary.Core.Kernel (Kernel, chain)

type S = Int

fancyKernel :: Int -> Kernel Int Int S
fancyKernel n a b = (a*b+n, a+b)

qcChain :: Int -> Int -> Int -> S -> S -> Bool
qcChain n0 n1 a0 s0 s1 = 
  let (a1,t0) = fancyKernel n0 a0 s0
      (a2,t1) = fancyKernel n1 a1 s1
  in chain fancyKernel [n0,n1] a0 [s0,s1] == (a2, [t0,t1])

testChainForSpaceLeak n = fst $ chain fancyKernel [1..n] 0 [1..n]
