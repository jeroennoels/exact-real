module Ternary.Mandelbrot where

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation
import Ternary.Sampling.Evaluation


step :: Num r => (r,r) -> (r,r) -> (r,r)
step (a,b) (x,y) = (x*x - y*y + a, 2*x*y + b)

numericMandel :: Num r => (r,r) -> [(r,r)]
numericMandel c = iterate (step c) c

mandelbrot :: Int -> Expr
mandelbrot = mandel (1:repeat 2) 

unsafeMandelbrot :: Int -> Expr
unsafeMandelbrot = mandel (2:repeat 4) 

mandel :: [Int] -> Int -> Expr
mandel normalizations depth = expression (vars ++ concat iterations)
  where
    vars = [(Ref 0, Id (Var 0)), (Ref 1, Id (Var 1))]
    iterations = zipWith iteration normalizations [0..depth-1]

absoluteRef :: Int -> Int -> Ref
absoluteRef k i = Ref (9*k + i)

xRef :: Int -> Ref
xRef k = absoluteRef (k-1) 0

yRef :: Int -> Ref
yRef k = absoluteRef (k-1) 1

iteration :: Int -> Int -> [(Ref, Node)]
iteration norm k =
  let rel = absoluteRef k
  in [(rel 2, Tims (rel 0) (rel 0)),  --  x^2
      (rel 3, Tims (rel 1) (rel 1)),  --  y^2
      (rel 4, Mins (rel 2) (rel 3)),  --  x^2 - y^2
      (rel 5, Tims (rel 0) (rel 1)),  --  x*y 
      (rel 6, Plus (rel 5) (rel 5)),  --  2*x*y
      (rel 7, Plus (rel 4) (Ref 0)),  --  new x
      (rel 8, Plus (rel 6) (Ref 1)),  --  new y
      (rel 9,  Norm (rel 7) norm),
      (rel 10, Norm (rel 8) norm)]

-- if
--     |x|,|y| < 3^p
--     |a|,|b| < 1
-- then
--     |new x| < 1 + 3^2p     <= 3^(2p+1)  
--     |new y| < 1 + 2 * 3^2p <= 3^(2p+1) 
