module Ternary.Sampling.Calculation where

import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map

import Data.Either
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map, insert, foldrWithKey', intersectionWith, (!))

import Ternary.Core.Digit
import Ternary.Core.Addition
import Ternary.Core.Multiplication
import Ternary.Compiler.ArrayState
import Ternary.Sampling.Expression
import Ternary.Util.Misc (strictlyIncreasing)

-- Because consumers may lag, each node needs to remember the complete
-- output it has produced thus far.

newtype Out = Out (Sequence.Seq T2) deriving Show

-- The following functions constitute the interface for the above
-- type.  To keep it simple, we shall not enforce this abstraction
-- with a typeclass or other means.

initOut :: Out
initOut = Out (Sequence.empty)

digitAt :: Out -> Int -> T2
digitAt (Out ds) i = Sequence.index ds i

append :: Out -> T2 -> Out
append (Out ds) d = Out (ds Sequence.|> d)

produced :: Out -> Int
produced (Out ds) = Sequence.length ds

outDigits :: Out -> [T2]
outDigits (Out ds) = toList ds

-- Our multiplication kernel has a 1-step delay, i.e. it only starts
-- producing output digits on the second step.  The first step is the
-- loading phase.  Thereafter it is ready to produce.

data Sm = Loading | Ready MulStateAS deriving Show

-- We reference a node from which to consume the output, and we track
-- how many of the available digits we have already consumed.

data Consumed = Consumed Ref Int deriving Show

-- The nodes and edges of the graph structure are annotated with the
-- data that represents a calculation in progress.

data NodeCalc = IdCalc Var Out
              | PlusCalc Consumed Consumed Sa Out
              | TimsCalc Consumed Consumed Sm Out
              deriving Show

data Calculation = Calc Ref (Map Ref NodeCalc)
                 deriving Show

-- keep the root
transform :: (Map Ref NodeCalc -> Map Ref NodeCalc) -> Calculation -> Calculation
transform f (Calc root nodes) = Calc root (f nodes) 

-- IdCalc nodes are at the bottom of the DAG. They receive "input"
-- from an external source.

isInput :: NodeCalc -> Bool
isInput (IdCalc _ _) = True
isInput _ = False

nodeOutput :: NodeCalc -> Out
nodeOutput (PlusCalc _ _ _ out) = out
nodeOutput (TimsCalc _ _ _ out) = out
nodeOutput (IdCalc _ out) = out

-- Remember that addition needs to prepend a certain number of zeros
-- to one of its arguments to cancel the difference in their offsets.
-- This is modeled with a negative number: (Consumed ref (-n)) means
-- that we shall first receive n additional zeros, before we start
-- consuming actual output from the node referenced by ref.

antiConsumed :: Ref -> Pre -> Consumed
antiConsumed ref (Pre n) = Consumed ref (-n)

initNodeCalc :: Node -> Shift -> NodeCalc
initNodeCalc (Id var) NoShift = IdCalc var initOut 
initNodeCalc (Plus a b) (ShiftPlus _ p q) =
  PlusCalc (antiConsumed a p) (antiConsumed b q) Sa0 initOut
initNodeCalc (Tims a b) _ =
  TimsCalc (Consumed a 0) (Consumed b 0) Loading initOut

initCalc :: Expr -> Calculation
initCalc x = Calc (rootRef x) calcMap
  where calcMap = intersectionWith initNodeCalc (nodes x) (shifts x)

-- This is the idea: after initialization, we want to progressively
-- refine a calculation.  Each step has a top-down phase followed by a
-- bottom-up phase.  During the top-down phase we identify the nodes
-- that need to be refined.  We say these nodes are "active".  During
-- the bottom-up phase, we attempt to refine the active nodes.  This
-- may occasionally fail because of delays.  In any case, at least one
-- active node shall make a state transition, so the calculation is
-- never stuck.  Let's start with the top-down phase.

-- A node is active when it cannot currently deliver the output that
-- is needed for the calculation to make progress.  We shall build two
-- separate lists of active nodes: one for input nodes (IdCalc) and one
-- for operations (PlusCalc, TimsCalc).

data Actives = Actives [(Ref, NodeCalc)] [(Ref, NodeCalc)]
             deriving Show

consActive :: (Ref, NodeCalc) -> Actives -> Actives
consActive node (Actives ins ops)
  | isInput (snd node) = Actives (node:ins) ops
  | otherwise = Actives ins (node:ops)

-- Node activation propagates top-down, so we start with the root.
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
activesAny p (Actives ins ops) = any p ins || any p ops
        
activatedBy :: (Ref, NodeCalc) -> NodeCalc -> Bool
child `activatedBy` PlusCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` TimsCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` IdCalc _ _ = False

exhausted :: (Ref, NodeCalc) -> Consumed -> Bool
exhausted (ref, child) (Consumed leg n) =
  ref == leg && produced (nodeOutput child) == n

-- Active nodes are consed onto a list while we perform a top-down
-- fold.  Therefor the resulting list is ordered bottom-up.  Here we
-- just provide the means to test this important property:

strictlyIncreasingRefs :: Actives -> Bool
strictlyIncreasingRefs (Actives _ ops) = strictlyIncreasing (map fst ops)

-- Now we enter the bottom-up phase.

consume :: Map Ref NodeCalc -> Consumed -> Maybe (T2, Consumed)
consume nodes (Consumed a p) 
  | p < 0 = Just (O0, done)
  | p < produced out = Just (out `digitAt` p, done)
  | otherwise = Nothing  -- exhausted
  where out = nodeOutput (nodes!a)
        done = Consumed a (p+1)


both :: Maybe a -> Maybe b -> Maybe (a,b)
both (Just a) (Just b) = Just (a,b)
both _ _ = Nothing

type DigitPairConsumed = ((T2, Consumed), (T2, Consumed))

consume2 :: Map Ref NodeCalc -> Consumed -> Consumed -> Maybe DigitPairConsumed
consume2 nodes a b = both (consume nodes a) (consume nodes b)


refineOperation :: Map Ref NodeCalc -> NodeCalc -> NodeCalc
--
refineOperation nodes
  orig@(PlusCalc a b old out) = maybe orig result (consume2 nodes a b)
  where
    result :: DigitPairConsumed -> NodeCalc
    result ((u,c),(v,d)) =
      let (w,new) = plus (addT2 u v) old
      in w `seq` PlusCalc c d new (append out w)
--
refineOperation nodes
  orig@(TimsCalc a b Loading out) = maybe orig result (consume2 nodes a b)
  where
    result :: DigitPairConsumed -> NodeCalc
    result ((u,c),(v,d)) =
      let new = initialMultiplicationState (TriangleParam u v)
      in TimsCalc c d (Ready new) out
--
refineOperation nodes
  orig@(TimsCalc a b (Ready old) out) = maybe orig result (consume2 nodes a b)
  where
    result :: DigitPairConsumed -> NodeCalc
    result ((u,c),(v,d)) =
      let (w,new) = kernel (u,v) old
      in  w `seq` TimsCalc c d (Ready new) (append out w)
--
refineOperation _ _ = error "New input is needed to refine an Id node"


refineNode :: Map Ref NodeCalc -> (Ref, NodeCalc) -> Map Ref NodeCalc
refineNode acc (ref,node) = insert ref (refineOperation acc node) acc

-- The above node-refiner is folded over the list of active operation
-- nodes.  Remember, by construction this list is ordered in a way
-- that corresponds to moving upwards in the DAG.  This is crucial,
-- because the outputs of these refinements will be consumed during
-- some subsequent refinement within the same traversal.

refineCalculation :: Actives -> Calculation -> Calculation
refineCalculation (Actives ins ops) calc@(Calc root nodes)
  | null ins = Calc root (foldl' refineNode nodes ops)
  | otherwise = error "refineCalculation: active inputs"

-- When there are active input nodes, the calculation must be
-- interrupted to provide the input needed to continue:

newtype Refined = Refined Calculation
data NeedsInput = NeedsInput Calculation Actives
data Continue = Continue Calculation [(Ref, NodeCalc)] 

variables :: NeedsInput -> [Var]
variables (NeedsInput _ (Actives ins _)) = map (extract . snd) ins
  where extract (IdCalc var _) = var
        
refine :: Refined -> Either Refined NeedsInput
refine (Refined calc)
  | null ins =   Left $ Refined (refineCalculation actives calc)
  | otherwise = Right $ NeedsInput calc actives
  where actives@(Actives ins _) = activeNodes calc

continue :: Continue -> Refined
continue (Continue calc ops) =
  Refined (refineCalculation (Actives [] ops) calc)

provideInput :: (Var -> T2) -> NeedsInput -> Continue
provideInput binding (NeedsInput calc (Actives ins ops)) =
  Continue (foldl' (flip $ refineInput binding) calc ins) ops

nodeInput :: (Var -> T2) -> NodeCalc -> NodeCalc
nodeInput binding (IdCalc var out) = IdCalc var (out `append` binding var)

refineInput :: (Var -> T2) -> (Ref, NodeCalc) -> Calculation -> Calculation
refineInput binding (ref, node) = transform $ insert ref (nodeInput binding node)

output :: Refined -> [T2]
output (Refined (Calc root nodes)) = outDigits $ nodeOutput (nodes!root)

rootOffset :: Integral i => Expr -> i
rootOffset x = offset (shifts x ! rootRef x)
