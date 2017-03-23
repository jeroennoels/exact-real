module Ternary.TestExpression where

import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Control.Monad (liftM, liftM2, sequence)

import Ternary.Core.Digit
import Ternary.Arbitraries
import Ternary.Util.Triad
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression


arbitraryNode :: Int -> Gen Node
arbitraryNode n = liftM2 Plus below below
  where below = choose (0,n-1)

arbitraryRefNode :: Int -> Gen (Ref,Node)
arbitraryRefNode n = return n `pairM` arbitraryNode n
  where pairM = liftM2 (,)
   
arbitraryRefNodes :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes n = sequence $ leaf : map arbitraryRefNode [1..n]
  where leaf = return (0,Id)

-- The list is traversed backwards (top-down) to accumulate all nodes
-- that are directly or indirectly referenced by the root node.
pruneList :: [(Ref,Node)] -> [(Ref,Node)]
pruneList list = recurse backwards [root]
  where (root:backwards) = reverse list
        recurse [] acc = acc
        recurse (a@(ref,node):rest) acc =
          recurse rest $ if any (references ref) acc
                         then (a:acc) else acc

references :: Ref -> (Ref,Node) -> Bool
references ref (_, Plus a b) = ref == a || ref == b
references ref (_, Id) = False
                         
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
qcEvaluate (Prune x y) = smartEval x 1 == smartEval y 1

qcActiveNodesPrune :: Prune -> Bool
qcActiveNodesPrune (Prune x y) =
  activeNodes (initCalc x) == activeNodes (initCalc y)

qcActiveNodesBottomUp :: Expr -> Bool
qcActiveNodesBottomUp = strictlyIncreasing . activeNodes . initCalc 
                       
expressionTest = quickBatch $
  ("Basic properties of arithmetical expressions",
   [("Evaluation", property qcEvaluate),
    ("Active nodes 1", property qcActiveNodesPrune),
    ("Active nodes 2", property qcActiveNodesBottomUp),
    ("Finite evaluation", property qcEval)])

qcEval :: Expr -> [T2] -> Bool
qcEval expr as = direct == finiteExactToTriad (unsafeFinite result)
  where direct = smartEval expr (phi as)
        result = Exact (evalFinite expr bs) p
        bs = as ++ replicate (maxHeight expr) O0
        p = rootOffset expr
