module Ternary.Mandelbrot where

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation hiding (Depth, refine)
import Ternary.Sampling.Evaluation


step :: Num r => (r,r) -> (r,r) -> (r,r)
step (a,b) (x,y) = (x*x - y*y + a, 2*x*y + b)

numericMandel :: Num r => (r,r) -> [(r,r)]
numericMandel c = iterate (step c) c

mandelbrot :: Int -> Expr
mandelbrot = mandel (1:repeat 2)

unsafeMandelbrot :: Int -> Expr
unsafeMandelbrot = mandel (repeat 4)

mandel :: [Int] -> Int -> Expr
mandel normalizations depth = expression (vars ++ scale ++ concat iterations)
  where
    vars = [(Ref 0, Id (Var 0)), (Ref 1, Id (Var 1))]
    double i = (absoluteRef 1 i, Plus (Ref i) (Ref i))
    scale = [double 0, double 1]
    iterations = zipWith iteration normalizations [1..depth]

absoluteRef :: Int -> Int -> Ref
absoluteRef k i = Ref (9*k + i)

iteration :: Int -> Int -> [(Ref, Node)]
iteration norm k =
  let rel = absoluteRef k
  in [(rel 2, Tims (rel 0) (rel 0)),  --  x^2
      (rel 3, Tims (rel 1) (rel 1)),  --  y^2
      (rel 4, Mins (rel 2) (rel 3)),  --  x^2 - y^2
      (rel 5, Tims (rel 0) (rel 1)),  --  x*y
      (rel 6, Plus (rel 5) (rel 5)),  --  2*x*y
      (rel 7, Plus (rel 4) (Ref 9)),  --  new x
      (rel 8, Plus (rel 6) (Ref 10)), --  new y
      (rel 9,  Norm (rel 7) norm),
      (rel 10, Norm (rel 8) norm)]

-- if
--     |x|,|y| < 3^p
--     |a|,|b| < 2
-- then
--     |new x| < 2 + 3^2p     <= 3^(2p+1)
--     |new y| < 2 + 2 * 3^2p <= 3^(2p+1)

xRef :: Int -> Ref
xRef k = absoluteRef k 7

yRef :: Int -> Ref
yRef k = absoluteRef k 8

sampleMandelbrot limit = undefined
  where
    expr = unsafeMandelbrot limit
    init = Refined (initCalc expr)


newtype SampleIn = SI [T2] deriving Show
type ComplexIn = (SampleIn, SampleIn)
type Acc = [ComplexIn]
data Refinable = Ready1 | Ready2 deriving Show
data Stalled = Stall deriving Show

type Depth = Int

cons :: T2 -> SampleIn -> SampleIn
cons a (SI as) = SI (a:as)

data Analysis = IncDepth | Bailout | Inconclusive
              deriving Show

limit = 2

analyze :: Depth -> Refinable -> Analysis
analyze depth _ | depth > limit = Bailout  
analyze _ Ready1 = Inconclusive
analyze _ Ready2 = IncDepth

proceed :: Binding T2 -> Stalled -> Either Refinable Stalled
proceed _ Stall = Left Ready2

refine :: Refinable -> Either Refinable Stalled
refine Ready1 = Right Stall
refine Ready2 = Left Ready1

recurse :: ComplexIn -> Depth -> Either Refinable Stalled -> Acc -> Acc
recurse c depth (Left refinable) acc =
  case analyze depth refinable of
   Bailout -> c:acc
   IncDepth -> recurse c (depth+1) (refine refinable) acc 
   Inconclusive -> recurse c depth (refine refinable) acc
recurse c depth (Right refinable) acc =
  let go :: Acc -> (ComplexIn, Binding T2) -> Acc
      go accum (extended, binding) =
        recurse extended depth (proceed binding refinable) accum
  in foldl go acc (branching c)


prepare :: ComplexIn -> T2 -> (ComplexIn, Binding T2)
prepare (a,b) d = ((cons d a, b), const d)

branching :: ComplexIn -> [(ComplexIn, Binding T2)]
branching c = map (prepare c) [M1, O0, P1]

-- recurse (SI [], SI []) 1 Ready1 []
