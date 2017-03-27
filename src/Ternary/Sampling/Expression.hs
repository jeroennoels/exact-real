{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Expression where

import Data.Maybe
import Control.Monad
import Data.Map.Strict (Map, insert, empty, fromList,
                        foldlWithKey', foldrWithKey',
                        intersectionWith, (!))
import qualified Data.Map.Strict as Map

import Ternary.Core.Digit
import Ternary.Core.Addition

type Ref = Int
data Node = Id | Plus Ref Ref deriving Show
data Expr = Expr Ref (Map Ref Node) deriving Show


-- The parent reference must be greater than its child references.
-- Thus the graph is acyclic and the nodes are topologically sorted.
monotonic :: (Ref, Node) -> Bool
monotonic (c, Plus a b) = a < c && b < c
monotonic (_, Id) = True

-- Verify that the nodes are topologically sorted.
-- The last one becomes the root.
expression :: [(Ref, Node)] -> Expr
expression assoc
  | all monotonic assoc = Expr root (fromList assoc)
  | otherwise = error "Ternary.Sampling.Expression: assert topological sort"
  where (root,_) = last assoc
        
example :: Expr
example = expression [(0, Id),
                      (1, Plus 0 0),
                      (2, Plus 1 1),
                      (3, Plus 2 1)]

maxHeight :: Expr -> Int
maxHeight (Expr _ nodes) = Map.size nodes

-- A small graph can represent a big tree.  Beware the fibonacci trap!
extreme :: Int -> Expr
extreme depth = expression assoc      
  where pair ref = (ref+1, Plus ref ref)
        assoc = (0,Id) : map pair [0..depth]

-- Scan the map in ascending key order.  Build a new map with exactly
-- the same domain.
mapScanL :: Ord k => (Map k b -> a -> b) -> Map k a -> Map k b
mapScanL f = foldlWithKey' op empty
  where op m k a = insert k (f m a) m
      
-- The naive recursive algoritm does not take advantage of sharing.
naiveEval :: Num a => Expr -> a -> a
naiveEval (Expr root nodes) x = eval (nodes!root)
  where eval (Plus a b) = eval (nodes!a) + eval (nodes!b)
        eval Id = x

-- Using the mapScanL combinator is easy and efficient.  It works
-- because the nodes are ordered, bottom up.
smartEval :: Num a => Expr -> a -> a
smartEval (Expr root nodes) x = mapScanL eval nodes ! root
  where eval m (Plus a b) = m!a + m!b
        eval _ Id = x

slowEvalExample = naiveEval (extreme 30) 1    -- takes forever
fastEvalExample = smartEval (extreme 1000) 1  -- no problem


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
        shift _ Id = NoShift


newtype Out = Out [T2] deriving (Show, Eq)

data Consumed = Consumed Ref Int deriving (Show, Eq)

data NodeCalc = IdCalc Out | PlusCalc Consumed Consumed Sa Out
              deriving (Show, Eq)

data Calculation = Calc Ref (Map Ref NodeCalc)
                 deriving Show

transform :: (Map Ref NodeCalc -> Map Ref NodeCalc) -> Calculation -> Calculation
transform f (Calc root nodes) = Calc root (f nodes) 

nodeOutput :: NodeCalc -> Out
nodeOutput (PlusCalc _ _ _ out) = out
nodeOutput (IdCalc out) = out

initOut :: Out
initOut = Out []
  
initConsumed :: Ref -> Pre -> Consumed
initConsumed ref (Pre n) = Consumed ref (-n)

initNodeCalc :: Node -> Shift -> NodeCalc
initNodeCalc Id NoShift = IdCalc initOut 
initNodeCalc (Plus a b) (ShiftPlus _ p q) =
  PlusCalc (initConsumed a p) (initConsumed b q) Sa0 initOut

initCalc :: Expr -> Calculation
initCalc (Expr root nodes) =
  Calc root $ intersectionWith initNodeCalc nodes (toShifts nodes)

activeNodes :: Calculation -> [(Ref, NodeCalc)]
activeNodes (Calc root nodes) =
  foldrWithKey' (curry accumulateActive) init unrooted
  where init = [(root, nodes!root)]  -- include these in the Calculation type?
        unrooted = Map.delete root nodes 

accumulateActive :: (Ref, NodeCalc) -> [(Ref, NodeCalc)] -> [(Ref, NodeCalc)]
accumulateActive cand acc =
  if any activated acc then cand:acc else acc
  where activated = activatedBy cand . snd
        
activatedBy :: (Ref, NodeCalc) -> NodeCalc -> Bool
child `activatedBy` PlusCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` IdCalc _ = error "Id node is a leaf"
 
exhausted :: (Ref, NodeCalc) -> Consumed -> Bool
exhausted (ref, child) (Consumed leg n) = ref == leg && length produced == n
  where Out produced = nodeOutput child

activeNodesExample :: Int -> [Ref]
activeNodesExample = map fst . activeNodes . initCalc . extreme 

strictlyIncreasing :: [(Ref, NodeCalc)] -> Bool
strictlyIncreasing list = and $ zipWith (<) refs (tail refs)
  where refs = map fst list

-- Precondition: the length of the output is greater than the number
-- that is already consumed
consume :: Map Ref NodeCalc -> Consumed -> (T2, Consumed)
consume nodes (Consumed a p) = (result, Consumed a (p+1))
  where result = if p < 0 then O0 else ds !! idx
        Out ds = nodeOutput (nodes!a)
        idx = length ds - 1 - p
        
        
refine :: Map Ref NodeCalc -> NodeCalc -> Maybe NodeCalc
refine _ (IdCalc _) = Nothing  -- new input is needed to refine an Id node
refine nodes (PlusCalc a1 a2 old (Out ds)) =
  Just $ PlusCalc c1 c2 new (Out (d:ds))
  where (d1,c1) = consume nodes a1
        (d2,c2) = consume nodes a2
        (d,new) = plus (addT2 d1 d2) old

update :: Map Ref NodeCalc -> (Ref, NodeCalc) -> Maybe (Map Ref NodeCalc)
update acc (ref,node) = flip (insert ref) acc `fmap` refine acc node

refineCalculation :: Calculation -> Maybe Calculation
refineCalculation calc@(Calc root nodes) =
  Calc root `fmap` foldM update nodes (activeNodes calc)

nodeInput :: T2 -> NodeCalc -> NodeCalc
nodeInput d (IdCalc (Out ds)) = IdCalc (Out (d:ds))
nodeInput _ node = node

refineInput :: T2 -> Calculation -> Calculation
refineInput = transform . Map.map . nodeInput

output :: Calculation -> [T2]
output (Calc root nodes) = reverse ds
  where Out ds = nodeOutput (nodes!root)

rootOffset :: Integral i => Expr -> i
rootOffset (Expr root nodes) = offset (toShifts nodes ! root)

-- Quick and dirty eval function to get first QuickCheck feedback.
-- To be improved later.
evalFinite :: Expr -> [T2] -> [T2]
evalFinite expr as = recurse (initCalc expr) as
  where recurse calc [] = output calc
        recurse calc input@(a:as) =
          let refined = refineCalculation calc
          in if isNothing refined
             then recurse (refineInput a calc) as 
             else recurse (fromJust refined) input
