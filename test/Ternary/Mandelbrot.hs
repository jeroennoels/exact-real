{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ternary.Mandelbrot where

import Control.Applicative (liftA2)

import Ternary.Core.Digit
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Util.Triad
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

sampleMandelbrot = recurse (Walk [] 0, Walk [] 0) 2 (Right init) []
  where
    expr = unsafeMandelbrot limit
    init = C.Refined (C.initCalc expr)

alternate :: C.Refined -> Depth -> (Ref, Ref)
alternate r depth =
  let x = C.output (xRef depth) r
      y = C.output (yRef depth) r
  in if length x <= length y -- todo swapIf
     then (xRef depth, yRef depth)
     else (yRef depth, xRef depth)


instance Refinable C.Refined C.NeedsInput where
   refine r depth = C.refine x r >>= C.refine y
     where (x,y) = alternate r depth
   proceed binding = Right . C.continue . C.provideInput binding
   variables = C.variables

instance Analyze C.Refined where
  analyze depth _ | depth > limit = Bailout
  analyze depth r =
    let x = C.output (xRef depth) r
        y = C.output (yRef depth) r
        escapes = absExceedsTwo x `orMaybe` absExceedsTwo y
    in case escapes of
        Just True -> Bailout
        Just False -> IncDepth
        Nothing -> Inconclusive

-- 3VL disjunction
orMaybe :: Maybe Bool -> Maybe Bool -> Maybe Bool
orMaybe (Just True) _ = Just True
orMaybe _ (Just True) = Just True
orMaybe (Just False) (Just False) = Just False
orMaybe _ _ = Nothing

-- assuming offset = 5
absExceedsTwo :: [T2] -> Maybe Bool
absExceedsTwo ds
  | a > 2 + epsilon = Just True 
  | a <= 2 - epsilon = Just False
  | otherwise = Nothing 
  where x = unsafeFinite $ Exact ds 5
        a = abs (finiteExactToTriad x)
        epsilon = makeTriad 243 (fromIntegral $ length ds) 

-- assuming offset = 1
absExceedsTwo' :: [T2] -> Maybe Bool 
absExceedsTwo' (P1:_) = Just False 
absExceedsTwo' (O0:_) = Just False 
absExceedsTwo' (M1:_) = Just False 
absExceedsTwo' (P2:ds) = case firstNonZero ds of
  Just d -> Just (isPositive d)
  Nothing -> Nothing
absExceedsTwo' (M2:ds) = case firstNonZero ds of
  Just d -> Just (isNegative d)
  Nothing -> Nothing
absExceedsTwo' [] = Nothing


isPositive :: T2 -> Bool
isPositive P2 = True
isPositive P1 = True
isPositive _ = False

isNegative :: T2 -> Bool
isNegative M2 = True
isNegative M1 = True
isNegative _ = False

firstNonZero :: [T2] -> Maybe T2
firstNonZero (O0:ds) = firstNonZero ds
firstNonZero (d:_) = Just d
firstNonZero _ = Nothing

keyValue :: (Walk2, Depth) -> (([T2],[T2]), Int)
keyValue ((Walk as _, Walk bs _), n) = ((reverse bs, reverse as), n)

accToMap :: Acc -> Map ([T2],[T2]) Int
accToMap = fromList . map keyValue

sortWalk :: Acc -> [Int]
sortWalk = map snd . toAscList . accToMap

toImage :: [Int] -> [String]
toImage = chunksOf (3^(logres+1)) . map toChar
  where toChar a | a > limit = ' '
                 | otherwise = '#'

