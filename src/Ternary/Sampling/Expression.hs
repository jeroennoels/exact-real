{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Expression (
  Ref, Pre(Pre), Off(Off), Var(Var),
  Expr(), Node(..), Shift(..),
  expression, nodes, shifts, rootRef, offset,
  extreme, smartEval, maxHeight,
  varX, varY, unsafeBind, xBind1) where

import Data.Maybe (fromJust, mapMaybe)
import Data.List (sort)
import Data.Map.Strict (Map, (!), insert, empty, fromList, foldlWithKey', findMax)
import qualified Data.Map.Strict as Map


newtype Var = Var Int deriving (Eq, Ord, Show)

type Binding a = Var -> a

varX = Var 0
varY = Var 1

unsafeBind :: [(Var,a)] -> Binding a
unsafeBind env var = fromJust (lookup var env)

bindAll :: [a] -> Binding a
bindAll values (Var i) = values !! i 

-- TODO consider newtype with Integral and Show instances.
type Ref = Int

data Node = Id Var | Plus Ref Ref deriving Show

data Expr = Expr {
  arity :: Int,
  rootRef :: Ref,
  nodes :: Map Ref Node,
  shifts :: Map Ref Shift } deriving Show

-- The parent reference must be greater than its child references.
-- Thus the graph is acyclic and the nodes are topologically sorted.
monotonic :: (Ref, Node) -> Bool
monotonic (c, Plus a b) = a < c && b < c
monotonic (_, Id _) = True

assertTopologicallySorted :: [(Ref, Node)] -> [(Ref, Node)]
assertTopologicallySorted assoc
  | all monotonic assoc = assoc
  | otherwise = error "Ternary.Sampling.Expression: assert topological sort"

extractVar :: (Ref, Node) -> Maybe Var 
extractVar (_, Id var) = Just var 
extractVar _ = Nothing

assertArityConvention :: [(Ref, Node)] -> Int
assertArityConvention assoc
  | sort vars == map Var [0..(n-1)] = n
  | otherwise = error $ "Ternary.Sampling.Expression: assert arity convention" ++ show (sort vars)
  where vars = mapMaybe extractVar assoc
        n = length vars
        
        
-- The greatest Ref value becomes the root. 
expression :: [(Ref, Node)] -> Expr
expression assoc = nodesMap `seq` verifiedArity `seq` Expr {
  arity = verifiedArity,
  rootRef = fst $ findMax nodesMap,
  nodes = nodesMap, 
  shifts = toShifts nodesMap }
  where
    verifiedArity = assertArityConvention assoc
    nodesMap = fromList $ assertTopologicallySorted assoc
                 
example :: Expr
example = expression [(0, Id varX),
                      (1, Plus 0 0),
                      (2, Plus 1 1),
                      (3, Plus 2 1)]

-- Crude upper bound
maxHeight :: Expr -> Int
maxHeight = Map.size . nodes

-- A small graph can represent a big tree.  Beware the fibonacci trap!
extreme :: Int -> Expr
extreme depth = expression assoc      
  where pair ref = (ref+1, Plus ref ref)
        assoc = (0, Id varX) : map pair [0..depth]

-- Scan the map in ascending key order.  Build a new map with exactly
-- the same domain.
mapScanL :: Ord k => (Map k b -> a -> b) -> Map k a -> Map k b
mapScanL f = foldlWithKey' op empty
  where op m k a = insert k (f m a) m
      
-- The naive recursive algoritm does not take advantage of sharing.
naiveEval :: Num a => Expr -> Binding a -> a
naiveEval (Expr _ root nodes _) binding = eval (nodes!root)
  where eval (Plus a b) = eval (nodes!a) + eval (nodes!b)
        eval (Id var) = binding var

-- Using the mapScanL combinator is easy and efficient.  It works
-- because the nodes are ordered, bottom up.
smartEval :: Num a => Expr -> Binding a -> a
smartEval (Expr _ root nodes _) binding = mapScanL eval nodes ! root
  where eval m (Plus a b) = m!a + m!b
        eval _ (Id var) = binding var


xBind1 = unsafeBind [(varX,1)]
slowEvalExample = naiveEval (extreme 30) xBind1    -- takes forever
fastEvalExample = smartEval (extreme 1000) xBind1  -- no problem


newtype Off = Off Int deriving Show
newtype Pre = Pre Int deriving Show

data Shift = NoShift | ShiftPlus Off Pre Pre deriving Show

offset :: Integral i => Shift -> i 
offset (ShiftPlus (Off p) _ _) = fromIntegral p
offset NoShift = 0

shiftPlus :: Shift -> Shift -> Shift
shiftPlus sx sy = ShiftPlus (Off (s+1)) (Pre (s-p)) (Pre (s-q))
  where p = offset sx
        q = offset sy
        s = max p q

-- Remember this only works if the nodes are ordered, bottom up.
toShifts :: Map Ref Node -> Map Ref Shift
toShifts = mapScanL shift
  where shift m (Plus a b) = shiftPlus (m!a) (m!b)
        shift _ (Id _) = NoShift
