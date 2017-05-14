module Ternary.Sampling.Calculation where

import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map

import Data.Either
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map, insert, foldrWithKey', intersectionWith, (!))
import Control.Arrow (first, second)

import Ternary.Core.Digit
import Ternary.Core.Addition
import Ternary.Core.Multiplication
import Ternary.Compiler.ArrayState
import Ternary.Sampling.Expression
import Ternary.Util.Misc (both, strictlyIncreasing)

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
              | MinsCalc Consumed Out
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
nodeOutput (MinsCalc _ out) = out
nodeOutput (IdCalc _ out) = out

-- Remember that addition needs to prepend a certain number of zeros
-- to one of its arguments to cancel the difference in their offsets.
-- This is modeled with a negative number: (Consumed ref (-n)) means
-- that we shall first receive n additional zeros, before we start
-- consuming actual output from the node referenced by ref.

antiConsumed :: Ref -> Pre -> Consumed
antiConsumed ref (Pre n) = Consumed ref (-n)

initNodeCalc :: Node -> Shift -> NodeCalc
initNodeCalc (Id var) _ = IdCalc var initOut
initNodeCalc (Mins a) _ = MinsCalc (Consumed a 0) initOut
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
-- for operations (PlusCalc, TimsCalc, etc.)

newtype ActiveIns = ActiveIns [(Ref, NodeCalc)] deriving Show
newtype ActiveOps = ActiveOps [(Ref, NodeCalc)] deriving Show

consIns :: (Ref, NodeCalc) -> ActiveIns -> ActiveIns
consIns node (ActiveIns ins) = ActiveIns (node:ins)

consOps :: (Ref, NodeCalc) -> ActiveOps -> ActiveOps
consOps node (ActiveOps ops) = ActiveOps (node:ops)

type Actives = (ActiveIns, ActiveOps)

consActive :: (Ref, NodeCalc) -> Actives -> Actives
consActive node
  | isInput (snd node) = first (consIns node)
  | otherwise = second (consOps node)

-- Node activation propagates top-down, so we start with the top.
-- Nota bene: the top can be any node, the root is not that special!

activesTop :: Calculation -> Ref -> Actives
activesTop (Calc _ nodes) top = consActive (top, nodes!top) empty
  where empty = (ActiveIns [], ActiveOps [])

activeNodes :: Calculation -> Ref -> Actives
activeNodes calc@(Calc root nodes) top
  | root == top = go nodes -- no need to split the map
  | otherwise   = go topdown
  where
    go = foldrWithKey' (curry accumulateActive) (activesTop calc top)
    topdown = fst $ Map.split top nodes -- top is excluded: not important here

accumulateActive :: (Ref, NodeCalc) -> Actives -> Actives
accumulateActive cand acc =
  if activesAny activated acc then consActive cand acc else acc
  where activated = activatedBy cand . snd

activesAny :: ((Ref, NodeCalc) -> Bool) -> Actives -> Bool
activesAny p (ActiveIns ins, ActiveOps ops) = any p ins || any p ops

activatedBy :: (Ref, NodeCalc) -> NodeCalc -> Bool
child `activatedBy` PlusCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` TimsCalc a b _ _ = exhausted child a || exhausted child b
child `activatedBy` MinsCalc a _ = exhausted child a
child `activatedBy` IdCalc _ _ = False

exhausted :: (Ref, NodeCalc) -> Consumed -> Bool
exhausted (ref, child) (Consumed leg n) =
  ref == leg && produced (nodeOutput child) == n

-- Active nodes are consed onto a list while we perform a top-down
-- fold.  Therefor the resulting list is ordered bottom-up.  Here we
-- just provide the means to test this important property:

strictlyIncreasingRefs :: ActiveOps -> Bool
strictlyIncreasingRefs (ActiveOps ops) = strictlyIncreasing (map fst ops)

-- Now we enter the bottom-up phase.

consume :: Map Ref NodeCalc -> Consumed -> Maybe (T2, Consumed)
consume nodes (Consumed a p)
  | p < 0 = Just (O0, done)
  | p < produced out = Just (out `digitAt` p, done)
  | otherwise = Nothing  -- exhausted
  where out = nodeOutput (nodes!a)
        done = Consumed a (p+1)


type DigitPairConsumed = ((T2, Consumed), (T2, Consumed))

consume2 :: Map Ref NodeCalc -> Consumed -> Consumed -> Maybe DigitPairConsumed
consume2 nodes a b = both (consume nodes a) (consume nodes b)


refineOperation :: Map Ref NodeCalc -> NodeCalc -> NodeCalc
--
refineOperation nodes
  orig@(MinsCalc a out) = maybe orig result (consume nodes a)
  where
    result :: (T2, Consumed) -> NodeCalc
    result (u,c) = MinsCalc c (out `append` negateT2 u)
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
    -- When we encounter leading zeros, we propagate them to the
    -- output and the node can stay in the loading phase.  This
    -- reduces the size of the multiplication state!
    result :: DigitPairConsumed -> NodeCalc
    result ((O0,c),(O0,d)) = TimsCalc c d Loading (out `append` O0 `append` O0)
    result ((O0,c),_) = TimsCalc c b Loading (out `append` O0)
    result (_,(O0,d)) = TimsCalc a d Loading (out `append` O0)
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
refineOperation _ _ = error "Ternary.Sampling.Calculation (refineOperation)"


refineNode :: Map Ref NodeCalc -> (Ref, NodeCalc) -> Map Ref NodeCalc
refineNode acc (ref,node) = insert ref (refineOperation acc node) acc

-- The above node-refiner is folded over the list of active operation
-- nodes.  Remember, by construction this list is ordered in a way
-- that corresponds to moving upwards in the DAG.  This is crucial,
-- because the outputs of these refinements will be consumed during
-- some subsequent refinement within the same traversal.

refineCalculation :: ActiveOps -> Calculation -> Calculation
refineCalculation (ActiveOps ops) = transform $ \nodes ->
  foldl' refineNode nodes ops

-- When there are active input nodes, the calculation must be
-- interrupted to provide the input needed to continue:

newtype Refined = Refined Calculation
data NeedsInput = NeedsInput Calculation Actives
data Continue = Continue Calculation ActiveOps

variables :: NeedsInput -> [Var]
variables (NeedsInput _ (ActiveIns ins, _)) = map (extract . snd) ins
  where extract (IdCalc var _) = var

refine :: Refined -> Ref -> Either Refined NeedsInput
refine (Refined calc) top
  | null ins =   Left $ Refined (refineCalculation ops calc)
  | otherwise = Right $ NeedsInput calc actives
  where actives@(ActiveIns ins, ops) = activeNodes calc top

continue :: Continue -> Refined
continue (Continue calc ops) = Refined (refineCalculation ops calc)

provideInput :: (Var -> T2) -> NeedsInput -> Continue
provideInput binding (NeedsInput calc (ActiveIns ins, ops)) =
  Continue (foldl' (flip $ refineInput binding) calc ins) ops

nodeInput :: (Var -> T2) -> NodeCalc -> NodeCalc
nodeInput binding (IdCalc var out) = IdCalc var (out `append` binding var)

refineInput :: (Var -> T2) -> (Ref, NodeCalc) -> Calculation -> Calculation
refineInput binding (ref, node) = transform $ insert ref (nodeInput binding node)

output :: Ref -> Refined -> [T2]
output ref (Refined (Calc _ nodes)) = outDigits $ nodeOutput (nodes!ref)

nodeOffset :: Integral i => Expr -> Ref -> i
nodeOffset x ref = offset (shifts x ! ref)
