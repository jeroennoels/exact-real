{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ternary.Mandelbrot where

import Control.Applicative (liftA2)

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Util.Triad
import Ternary.Util.Misc (orMaybe, swapIf)
import Ternary.Sampling.Expression
import qualified Ternary.Sampling.Calculation as C
import Ternary.Recursive

import Data.Map.Strict (Map, fromList, toAscList)
import Data.List.Split (chunksOf)


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

logres = 4
limit = 5

sampleMandelbrot = recurse (walk, walk) 1 (Right init) []
  where
    expr = unsafeMandelbrot limit
    walk = Branching [] logres
    init = C.Refined (C.initCalc expr)

-- We need to alternate the order in which the two output nodes are
-- refined.  Otherwise we keep refining one node, and remain forever
-- stuck on the other.

alternate :: C.Refined -> Depth -> (Ref, Ref)
alternate r depth =
  let lenx = C.produced $ C.output (xRef depth) r
      leny = C.produced $ C.output (yRef depth) r
  in (xRef depth, yRef depth) `swapIf` (lenx > leny)


instance Refinable C.Refined C.NeedsInput where
   refine r depth = C.refine x r >>= C.refine y
     where (x,y) = alternate r depth
   proceed binding = Right . C.continue . C.provideInput binding
   variables = C.variables

instance Analyze C.Refined where
  analyze depth _ | depth > limit = Bailout
  analyze depth r =
    let x = C.outputList (xRef depth) r
        y = C.outputList (yRef depth) r
        escapes = absExceedsTwo x `orMaybe` absExceedsTwo y
    in case escapes of
        Just True -> Bailout
        Just False -> IncDepth
        Nothing -> Inconclusive


-- assuming offset = 5
absExceedsTwo :: [T2] -> Maybe Bool
absExceedsTwo ds
  | a > 2 + epsilon = Just True 
  | a < 2 - epsilon = Just False
  | otherwise = Nothing 
  where x = unsafeFinite $ Exact ds 5
        a = abs (finiteExactToTriad x)
        epsilon = makeTriad 243 (fromIntegral $ length ds)


keyValue :: (Walk2, Depth) -> (([T2],[T2]), Int)
keyValue ((a,b),n) = ((path b, path a), n)

sortWalk :: Acc -> [Int]
sortWalk = map snd . toAscList . fromList . map keyValue

toImage :: [Int] -> [String]
toImage = chunksOf (3^logres) . map toChar
  where toChar a | a > limit = ' '
                 | otherwise = '#'
