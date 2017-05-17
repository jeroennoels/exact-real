module Ternary.Mandelbrot where

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation
import Ternary.Sampling.Evaluation

--  x(k+1) = x(k)^2 - y(k)^2 + re(c)
--  y(k+1) = 2 x(k) y(k) + im(c)

--  x(0) = 0
--  y(0) = 0

--  x(1) = re(c)
--  y(1) = im(c)

mandelbrot :: Int -> Expr
mandelbrot depth = expression (vars ++ concat iterations)
  where
    vars = [(Ref 0, Id (Var 0)), (Ref 1, Id (Var 1))]
    iterations = map iteration [0..depth-2]

absoluteRef :: Int -> Int -> Ref
absoluteRef k i = Ref (7*k + i)

xRef :: Int -> Ref
xRef k = absoluteRef (k-1) 0

yRef :: Int -> Ref
yRef k = absoluteRef (k-1) 1

iteration :: Int -> [(Ref, Node)]
iteration k =
  [(rel 2, Tims (rel 0) (rel 0)),  --  x^2
   (rel 3, Tims (rel 1) (rel 1)),  --  y^2
   (rel 4, Mins (rel 2) (rel 3)),  --  x^2 - y^2
   (rel 5, Tims (rel 0) (rel 1)),  --  x*y 
   (rel 6, Plus (rel 5) (rel 5)),  --  2*x*y
   (rel 7, Plus (rel 4) (Ref 0)),  --  new x
   (rel 8, Plus (rel 6) (Ref 1))]  --  new y
  where
    rel = absoluteRef k
