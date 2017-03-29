{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Expression where

import Data.Either
import Data.List (foldl')
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

data Actives = Actives [(Ref, NodeCalc)] [(Ref, NodeCalc)]
             deriving (Show, Eq)

getInputs :: Actives -> [(Ref, NodeCalc)]
getInputs (Actives inputs _) = inputs

getOthers :: Actives -> [(Ref, NodeCalc)]
getOthers (Actives _ others) = others

isInput :: (Ref, NodeCalc) -> Bool
isInput (_, IdCalc _) = True
isInput _ = False

consActive :: (Ref, NodeCalc) -> Actives -> Actives
consActive node (Actives inputs others)
  | isInput node = Actives (node:inputs) others
  | otherwise = Actives inputs (node:others)

-- Include this in the Calculation type?
activesRoot :: Calculation -> Actives
activesRoot (Calc root nodes) = consActive (root, nodes!root) (Actives [] [])

activeNodes :: Calculation -> Actives
activeNodes calc@(Calc root nodes) =
  foldrWithKey' (curry accumulateActive) (activesRoot calc) unrooted
  where unrooted = Map.delete root nodes 

accumulateActive :: (Ref, NodeCalc) -> Actives -> Actives
accumulateActive cand acc =
  if activesAny activated acc then consActive cand acc else acc
  where activated = activatedBy cand . snd

activesAny :: ((Ref, NodeCalc) -> Bool) -> Actives -> Bool
activesAny p (Actives inputs others) = any p inputs || any p others
        
activatedBy :: (Ref, NodeCalc) -> NodeCalc -> Bool
child `activatedBy` PlusCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` IdCalc _ = error "Id node is a leaf"
 
exhausted :: (Ref, NodeCalc) -> Consumed -> Bool
exhausted (ref, child) (Consumed leg n) = ref == leg && length produced == n
  where Out produced = nodeOutput child

activeNodesExample :: Int -> Actives
activeNodesExample = activeNodes . initCalc . extreme 

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
        
-- Precondition: the given node is not an input node
refineNode :: Map Ref NodeCalc -> NodeCalc -> NodeCalc
refineNode _ (IdCalc _) = error "New input is needed to refine an Id node"
refineNode nodes (PlusCalc a1 a2 old (Out ds)) =
  d `seq` PlusCalc c1 c2 new (Out (d:ds))
  where (d1,c1) = consume nodes a1
        (d2,c2) = consume nodes a2
        (d,new) = plus (addT2 d1 d2) old

update :: Map Ref NodeCalc -> (Ref, NodeCalc) -> Map Ref NodeCalc
update acc (ref,node) = flip (insert ref) acc (refineNode acc node)

refineCalculation :: Actives -> Calculation -> Calculation
refineCalculation (Actives inputs others) calc@(Calc root nodes)
  | null inputs = Calc root (foldl' update nodes others)
  | otherwise = error "refineCalculation: active inputs"

newtype Refinement = Refined Calculation
data NeedsInput = NeedsInput Calculation Actives
data Continue = Continue Calculation [(Ref, NodeCalc)] 
                  
refine :: Refinement -> Either Refinement NeedsInput
refine (Refined calc)
  | null inputs = Left $ Refined (refineCalculation actives calc)
  | otherwise =  Right $ NeedsInput calc actives
  where actives@(Actives inputs _) = activeNodes calc

continue :: Continue -> Refinement
continue (Continue calc others) =
  Refined (refineCalculation (Actives [] others) calc)

provideInput :: T2 -> NeedsInput -> Continue
provideInput d (NeedsInput calc (Actives [input] others)) =
  Continue (refineInput d input calc) others

nodeInput :: T2 -> NodeCalc -> NodeCalc
nodeInput d (IdCalc (Out ds)) = IdCalc (Out (d:ds))

refineInput :: T2 -> (Ref, NodeCalc) -> Calculation -> Calculation
refineInput d (ref, node) = transform $ insert ref (nodeInput d node)

output :: Refinement -> [T2]
output (Refined (Calc root nodes)) = reverse ds
  where Out ds = nodeOutput (nodes!root)

-- TODO avoid computing this a second time
rootOffset :: Integral i => Expr -> i
rootOffset (Expr root nodes) = offset (toShifts nodes ! root)

evalFinite :: Expr -> [T2] -> [T2]
evalFinite expr as = recurse (Refined (initCalc expr)) as
  where recurse refinement [] = output refinement
        recurse refinement input@(a:as) =
          let attempt = refine refinement
              done = either id (continue . provideInput a) attempt
          in recurse done (if isLeft attempt then input else as)

sample :: Expr -> Int -> [[T2]]
sample expr depth = recurse (Refined (initCalc expr)) depth []
  where recurse refinement 0 acc = output refinement : acc
        recurse refinement depth acc =
          let Right needsInput = refine refinement
              go a state = let done = continue (provideInput a needsInput)
                           in recurse done (depth-1) state
          in go M1 $ go O0 $ go P1 acc
              
