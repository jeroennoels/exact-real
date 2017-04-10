{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Evaluation where

import Data.Maybe (fromJust)

import Ternary.Core.Digit
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation

type VarAssign a = [(Var, a)]

unsafeBind :: VarAssign a -> Binding a
unsafeBind assoc var = fromJust (lookup var assoc)

evalFinite :: Expr -> VarAssign [T2] -> [T2]
evalFinite expr = recurse $ Refined (initCalc expr)
  where
    recurse refinement lists | inputExhausted lists = output refinement
    recurse refinement lists =
      case refine refinement of
       Left done -> recurse done lists
       Right ask -> recurse (continue (provideInput heads ask)) tails
         where (heads, tails) = unconsInputs lists (variables ask)

-- We stop as soon as one of the variables is fully consumed, even
-- when that variable is not immediately needed.  So we could do
-- better here.  But I prefer to keep it simple.
inputExhausted :: VarAssign [a] -> Bool
inputExhausted lists = any (null . snd) lists

-- Precondition: inputExhausted assoc == False
unconsInputs :: VarAssign [T2] -> [Var] -> (Binding T2, VarAssign [T2])
unconsInputs assoc vars = (binding, map consumed assoc)
  where
    consumed keep@(var, ds)    
      | var `elem` vars = (var, tail ds)
      | otherwise = keep
    binding var
      | var `elem` vars = head $ fromJust (lookup var assoc)

evalFinite1 :: Expr -> [T2] -> [T2]
evalFinite1 expr x = evalFinite expr [(Var 0, x)]


                 
example :: Expr
example = expression [(Ref 0, Id (Var 0)),
                      (Ref 1, Plus (Ref 0) (Ref 0)),
                      (Ref 2, Tims (Ref 1) (Ref 1)),
                      (Ref 3, Tims (Ref 2) (Ref 1))]
