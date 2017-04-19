{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Expression (
  Ref(Ref), Pre(Pre), Off(Off), Var(Var),
  Expr(), Node(..), Shift(..), Binding,
  expression, arity, nodes, shifts, rootRef, offset,
  mapScanL, extreme, smartEval, bind, bindAll) where

import Data.Maybe (fromJust, mapMaybe)
import Data.List (sort)
import Data.Map.Strict (Map, (!), insert, empty, fromList, foldlWithKey', findMax)
import qualified Data.Map.Strict as Map

-- To keep it simple, the variables of an expression of arity n are
-- labeled by numbers from 0 to n-1.
newtype Var = Var Int deriving (Eq, Ord, Show)

-- A binding is a function that assigns values to the variables of an
-- expression:
type Binding a = Var -> a

bind :: a -> Binding a
bind a (Var 0) = a

bindAll :: [a] -> Binding a
bindAll values (Var i) = values !! i

-- References point to shared sub-expressions.
newtype Ref = Ref Int deriving (Eq, Ord, Show)

data Node = Id Var
          | Plus Ref Ref
          | Tims Ref Ref
          deriving Show

-- The offset of an expression is essentially a positive base 3
-- exponent.  But I want to avoid the floating point connotation
-- because that is not the purpose of these offsets: they are merely
-- intended to compensate for the relative displacements caused by
-- addition and multiplication operations.  See also:
-- Ternary.List.Exact

newtype Off = Off Int deriving Show

-- Negative base 3 exponents are viewed as the number of zeros we
-- prefix to a stream of digits.

newtype Pre = Pre Int deriving Show

-- Offsets and prefixes follow the structure of the expression.

data Shift = NoShift
           | ShiftPlus Off Pre Pre
           | ShiftTims Off
           deriving Show

data Expr = Expr {
  arity :: Int,
  rootRef :: Ref,
  nodes :: Map Ref Node,
  shifts :: Map Ref Shift } deriving Show

-- The parent reference must be greater than its child references.
-- Thus the graph is acyclic and the nodes are topologically sorted.

monotonic :: (Ref, Node) -> Bool
monotonic (c, Plus a b) = a < c && b < c
monotonic (c, Tims a b) = a < c && b < c
monotonic (_, Id _) = True

-- We will do a lot of folding over the (Map Ref Node) that defines an
-- expression.  Thereby we shall crucially depend on the pre-condition
-- that this map is topologically sorted.  When traversed in ascending
-- Ref order, child nodes are always visited before their parents.
-- Conversely, when traversed in descending Ref order, the parent
-- comes first.  Thus left folding amounts to bottom-up aggregation
-- whereas right folds are top-down.

assertTopologicallySorted :: [(Ref, Node)] -> [(Ref, Node)]
assertTopologicallySorted assoc
  | all monotonic assoc = assoc
  | otherwise = error "Ternary.Sampling.Expression: assert topological sort"

extractVar :: Node -> Maybe Var
extractVar (Id var) = Just var
extractVar _ = Nothing

assertArityConvention :: [(Ref, Node)] -> Int
assertArityConvention assoc
  | sort vars == map Var [0..n-1] = n
  | otherwise = error $ "Ternary.Sampling.Expression: assert arity convention"
  where vars = mapMaybe (extractVar . snd) assoc
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

-- A small graph can represent a big tree.  Beware the fibonacci trap!
extreme :: Int -> Expr
extreme depth = expression assoc
  where pair i = (Ref (i+1), Plus (Ref i) (Ref i))
        assoc = (Ref 0, Id (Var 0)) : map pair [0..depth-1]

-- Scan the map in ascending key order.  Build a new map with exactly
-- the same domain.
mapScanL :: Ord k => (Map k b -> a -> b) -> Map k a -> Map k b
mapScanL f = foldlWithKey' op empty
  where op m k a = insert k (f m a) m

-- The naive top-down recursion does not take advantage of sharing.
naiveEval :: Num a => Expr -> Binding a -> a
naiveEval (Expr _ root nodes _) binding = eval (nodes!root)
  where eval (Plus a b) = eval (nodes!a) + eval (nodes!b)
        eval (Tims a b) = eval (nodes!a) * eval (nodes!b)
        eval (Id var) = binding var

-- Using the mapScanL combinator is easy and efficient.  It works
-- because the nodes are ordered, bottom-up.
smartEval :: Num a => Expr -> Binding a -> a
smartEval (Expr _ root nodes _) binding = mapScanL eval nodes ! root
  where eval m (Plus a b) = m!a + m!b
        eval m (Tims a b) = m!a * m!b
        eval _ (Id var) = binding var

slowEvalExample = naiveEval (extreme 30) (bind 1)    -- takes forever
fastEvalExample = smartEval (extreme 1000) (bind 1)  -- no problem


offset :: Integral i => Shift -> i
offset (ShiftPlus (Off p) _ _) = fromIntegral p
offset (ShiftTims (Off p)) = fromIntegral p
offset NoShift = 0

-- See Ternary.List.Exact (addExact)
shiftPlus :: Shift -> Shift -> Shift
shiftPlus sx sy = ShiftPlus (Off (s+1)) (Pre (s-p)) (Pre (s-q))
  where p = offset sx
        q = offset sy
        s = max p q

-- See Ternary.List.Exact (multiplyExact)
shiftTims :: Shift -> Shift -> Shift
shiftTims sx sy = ShiftTims (Off (p+q+1))
  where p = offset sx
        q = offset sy

-- Remember this only works if the nodes are ordered, bottom up.
toShifts :: Map Ref Node -> Map Ref Shift
toShifts = mapScanL shift
  where shift m (Plus a b) = shiftPlus (m!a) (m!b)
        shift m (Tims a b) = shiftTims (m!a) (m!b)
        shift _ (Id _) = NoShift
