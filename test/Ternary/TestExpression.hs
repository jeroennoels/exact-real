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
  where leaf = return (0, Id varX)

arbitraryRefNodes2 :: Int -> Gen [(Ref,Node)]
arbitraryRefNodes2 n = sequence $ leafX : leafY : map arbitraryRefNode [2..n]
  where leafX = return (0, Id varX)
        leafY = return (1, Id varY)

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
qcEvaluate (Prune x y) = smartEval x xBind1 == smartEval y xBind1

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
    ("Finite evaluation", property qcEval2)])

qcEval :: Expr -> [T2] -> Bool
qcEval expr as = direct == finiteExactToTriad (unsafeFinite result)
  where direct = smartEval expr binding
        binding = unsafeBind [(varX, phi as)]
        result = Exact (evalFinite expr bs) p
        bs = as ++ replicate (maxHeight expr) O0
        p = rootOffset expr


data Prune2 = Prune2 Expr Expr deriving Show

prune2 :: [(Ref,Node)] -> Prune2
prune2 list = expression list `Prune2` expression (pruneList list)
                  
instance Arbitrary Prune2 where 
  arbitrary = prune2 `liftM` (arbitrarySizedNatural >>= arbitraryRefNodes2)

qcEval2 :: Prune2 -> [T2] -> [T2] -> Bool
qcEval2 (Prune2 _ expr) as bs = direct == finiteExactToTriad (unsafeFinite result)
  where direct = smartEval expr binding
        binding = unsafeBind [(varX, phi as), (varY, phi bs)]
        result = Exact (evalFinite2 expr as' bs') p
        as' = as ++ replicate (length bs + maxHeight expr) O0
        bs' = bs ++ replicate (length as + maxHeight expr) O0
        p = rootOffset expr
