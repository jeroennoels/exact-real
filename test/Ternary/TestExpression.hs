module Ternary.TestExpression where

import Data.List (sortBy)
import Data.Function (on)
import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Control.Monad (liftM, liftM2, sequence)
import Data.Map.Strict ((!))

import Ternary.Core.Digit
import Ternary.Arbitraries
import Ternary.Util.Triad
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation
import Ternary.Sampling.Evaluation

example :: Expr
example = expression [(Ref 0, Id (Var 0)),
                      (Ref 1, Plus (Ref 0) (Ref 0)),
                      (Ref 2, Tims (Ref 1) (Ref 1)),
                      (Ref 3, Plus (Ref 2) (Ref 2)),
                      (Ref 4, Tims (Ref 3) (Ref 3)),
                      (Ref 5, Plus (Ref 0) (Ref 0)),
                      (Ref 6, Tims (Ref 5) (Ref 5)),
                      (Ref 7, Plus (Ref 4) (Ref 6))]

id0 = (Ref 0, Id (Var 0))
id1 = (Ref 1, Id (Var 1))

arbitraryNode :: Int -> Gen Node
arbitraryNode n = liftM2 (binop `on` Ref) below below
  where below = choose (0, n-1)
        binop = if mod n 4 == 0 then Tims else Plus

arbitraryRefNode :: Int -> Gen (Ref,Node)
arbitraryRefNode n = return (Ref n) `pairM` arbitraryNode n
  where pairM = liftM2 (,)

arbitraryRefNodes :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes n = sequence $ return id0 : map arbitraryRefNode [1..n]

-- To obtain an expression of arity 2, we must ensure both Id nodes
-- are actually reachable from the root.

arbitraryRefNodes2 :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes2 k = sequence list
  where
    n = k+2
    use0 = (Ref (n+1), Tims (Ref n) (Ref 0))
    use1 = (Ref (n+2), Plus (Ref (n+1)) (Ref 1))
    list = map return [id0, id1, use0, use1] ++ map arbitraryRefNode [2..n]

-- The list is traversed backwards (top-down) to accumulate all nodes
-- that are directly or indirectly referenced by the root node.
pruneList :: [(Ref,Node)] -> [(Ref,Node)]
pruneList list = recurse backwards [root]
  where (root:backwards) = sortBy (flip compare `on` fst) list
        recurse [] acc = acc
        recurse (a@(ref,node):rest) acc =
          recurse rest $ if any (references ref . snd) acc
                         then (a:acc) else acc

references :: Ref -> Node -> Bool
references ref (Plus a b) = ref == a || ref == b
references ref (Tims a b) = ref == a || ref == b
references ref (Id _) = False

-- Combine an expression and its pruned equivalent
data Prune = Prune Expr Expr deriving Show

prune :: [(Ref,Node)] -> Prune
prune list = expression list `Prune` expression (pruneList list)

prunedExpr :: Prune -> Expr
prunedExpr (Prune x y) = y

instance Arbitrary Prune where
  arbitrary = prune `liftM` (arbitrarySizedNatural >>= arbitraryRefNodes)

instance Arbitrary Expr where
  arbitrary = liftM prunedExpr arbitrary

qcEvaluate :: Prune -> Bool
qcEvaluate (Prune x y) = smartEval x (bind 1) == smartEval y (bind 1)

qcActiveNodesBottomUp :: Expr -> Bool
qcActiveNodesBottomUp = strictlyIncreasingRefs . snd . actives . initCalc
   where actives calc@(Calc root _) = activeNodes calc root
         
expressionTest = quickBatch $
  ("Basic properties of arithmetical expressions",
   [("Evaluation", property qcEvaluate),
    ("Active nodes", property qcActiveNodesBottomUp),
    ("Finite evaluation 1", property qcEval),
    ("Finite evaluation 2", property qcEval2)])

data Expr2 = Expr2 Expr deriving Show

expr2 :: [(Ref,Node)] -> Expr2
expr2 = Expr2 . expression . pruneList

instance Arbitrary Expr2 where
  arbitrary = expr2 `liftM` (arbitrarySizedNatural >>= arbitraryRefNodes2)

qcEval2 :: Expr2 -> [T2] -> Bool
qcEval2 (Expr2 expr) ds = qcEval expr ds

qcEval :: Expr -> [T2] -> Bool
qcEval expr ds = direct == finiteExactToTriad (unsafeFinite result)
  where
    direct = smartEval expr binding
    (va, binding) = buildVarAssign expr ds
    p = rootOffset expr
    result = Exact (evalFinite expr va) p

-- Not maximally random but good enough for now.
buildVarAssign :: Expr -> [T2] -> (VarAssign [T2], Binding Triad)
buildVarAssign expr as = (map assign [0..n], binding)
  where
    n = arity expr - 1
    k = significantDigits expr (const (1 + length as))
    zeros = replicate (k+1) O0
    digits i = let (bs,cs) = splitAt i as in cs ++ bs  -- shuffle
    assign i = (Var i, digits i ++ zeros)
    binding (Var i) = phi (digits i)


significantDigits :: (Ord a, Num a) => Expr -> Binding a -> a
significantDigits expr binding = mapScanL eval (nodes expr) ! rootRef expr
  where eval m (Plus a b) = 1 + max (m!a) (m!b)
        eval m (Tims a b) = 1 + m!a + m!b
        eval _ (Id var) = binding var
