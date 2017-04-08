module Ternary.TestExpression where

import Data.List (sortBy)
import Data.Function (on)
import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Binop)
import Control.Monad (liftM, liftM2, sequence)

import Ternary.Core.Digit
import Ternary.Arbitraries
import Ternary.Util.Triad
import Ternary.List.Exact
import Ternary.List.FiniteExact
import Ternary.Sampling.Expression
import Ternary.Sampling.Calculation
import Ternary.Sampling.Evaluation

id0 = (0, Id (Var 0))
id1 = (1, Id (Var 1))

arbitraryNode :: Int -> Gen Node
arbitraryNode n = liftM2 Plus below below
  where below = choose (0,n-1)

arbitraryRefNode :: Int -> Gen (Ref,Node)
arbitraryRefNode n = return n `pairM` arbitraryNode n
  where pairM = liftM2 (,)
   
arbitraryRefNodes :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes n = sequence $ return id0 : map arbitraryRefNode [1..n]

arbitraryRefNodes2 :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes2 n = sequence list
  where use0 = (n+1, Plus n 0)
        use1 = (n+2, Plus (n+1) 1)
        list = map return [id0, id1, use0, use1] ++ map arbitraryRefNode [2..n]

-- The list is traversed backwards (top-down) to accumulate all nodes
-- that are directly or indirectly referenced by the root node.
pruneList :: [(Ref,Node)] -> [(Ref,Node)]
pruneList list = recurse backwards [root]
  where (root:backwards) = sortBy (flip compare `on` fst) list
        recurse [] acc = acc
        recurse (a@(ref,node):rest) acc =
          recurse rest $ if any (references ref) acc
                         then (a:acc) else acc

references :: Ref -> (Ref,Node) -> Bool
references ref (_, Plus a b) = ref == a || ref == b
references ref (_, Id _) = False
                         
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

qcActiveNodesPrune :: Prune -> Bool
qcActiveNodesPrune (Prune x y) =
  activeNodes (initCalc x) == activeNodes (initCalc y)

qcActiveNodesBottomUp :: Expr -> Bool
qcActiveNodesBottomUp = strictlyIncreasing . getOthers . activeNodes . initCalc 
                       
expressionTest = quickBatch $
  ("Basic properties of arithmetical expressions",
   [("Evaluation", property qcEvaluate),
    ("Active nodes 1", property qcActiveNodesPrune),
    ("Active nodes 2", property qcActiveNodesBottomUp),
    ("Finite evaluation 1", property qcEval),
    ("Finite evaluation 2", property qcEval2)])

data Prune2 = Prune2 Expr Expr deriving Show

prune2 :: [(Ref,Node)] -> Prune2
prune2 list = expression list `Prune2` expression (pruneList list)
                  
instance Arbitrary Prune2 where 
  arbitrary = prune2 `liftM` (arbitrarySizedNatural >>= arbitraryRefNodes2)

qcEval2 :: Prune2 -> [T2] -> Bool
qcEval2 (Prune2 _ expr) ds = qcEval expr ds
  
qcEval :: Expr -> [T2] -> Bool
qcEval expr ds = direct == finiteExactToTriad (unsafeFinite result)
  where
    direct = smartEval expr binding
    binding = phi . unsafeBind va
    va = buildVarAssign expr ds
    p = rootOffset expr
    result = Exact (evalFinite expr va) p

-- Not maximally random but good enough for now.
buildVarAssign :: Expr -> [T2] -> VarAssign [T2]
buildVarAssign expr as = map assign [0..n] 
  where
    n = arity expr - 1
    bs = as ++ replicate (maxHeight expr) O0
    assign i = (Var i, drop i bs)
