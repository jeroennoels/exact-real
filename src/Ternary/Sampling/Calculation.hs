{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Sampling.Calculation where

import Data.Either
import Data.List (foldl')
import Data.Map.Strict (Map, insert, foldrWithKey', intersectionWith, (!))
import qualified Data.Map.Strict as Map

import Ternary.Core.Digit
import Ternary.Core.Addition
import Ternary.Sampling.Expression


newtype Out = Out [T2] deriving (Show, Eq)

data Consumed = Consumed Ref Int deriving (Show, Eq)

data NodeCalc = IdCalc Var Out | PlusCalc Consumed Consumed Sa Out
              deriving (Show, Eq)

data Calculation = Calc Ref (Map Ref NodeCalc)
                 deriving Show

transform :: (Map Ref NodeCalc -> Map Ref NodeCalc) -> Calculation -> Calculation
transform f (Calc root nodes) = Calc root (f nodes) 

nodeOutput :: NodeCalc -> Out
nodeOutput (PlusCalc _ _ _ out) = out
nodeOutput (IdCalc _ out) = out

initOut :: Out
initOut = Out []
  
initConsumed :: Ref -> Pre -> Consumed
initConsumed ref (Pre n) = Consumed ref (-n)

initNodeCalc :: Node -> Shift -> NodeCalc
initNodeCalc (Id var) NoShift = IdCalc var initOut 
initNodeCalc (Plus a b) (ShiftPlus _ p q) =
  PlusCalc (initConsumed a p) (initConsumed b q) Sa0 initOut

initCalc :: Expr -> Calculation
initCalc x = Calc (rootRef x) calcMap
  where calcMap = intersectionWith initNodeCalc (nodes x) (shifts x)

-- TODO Consider record syntax and improve encapsulation.
data Actives = Actives [(Ref, NodeCalc)] [(Ref, NodeCalc)]
             deriving (Show, Eq)

getInputs :: Actives -> [(Ref, NodeCalc)]
getInputs (Actives inputs _) = inputs

getOthers :: Actives -> [(Ref, NodeCalc)]
getOthers (Actives _ others) = others

isInput :: (Ref, NodeCalc) -> Bool
isInput (_, IdCalc _ _) = True
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
child `activatedBy` IdCalc _ _ = False
 
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
refineNode _ (IdCalc _ _) = error "New input is needed to refine an Id node"
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

extractVar :: (Ref, NodeCalc) -> Var
extractVar (_, IdCalc var _) = var

variables :: NeedsInput -> [Var]
variables (NeedsInput _ (Actives inputs _)) = map extractVar inputs

refine :: Refinement -> Either Refinement NeedsInput
refine (Refined calc)
  | null inputs = Left $ Refined (refineCalculation actives calc)
  | otherwise =  Right $ NeedsInput calc actives
  where actives@(Actives inputs _) = activeNodes calc

continue :: Continue -> Refinement
continue (Continue calc others) =
  Refined (refineCalculation (Actives [] others) calc)

provideInput :: (Var -> T2) -> NeedsInput -> Continue
provideInput binding (NeedsInput calc (Actives inputs others)) =
  Continue (foldl' (flip $ refineInput binding) calc inputs) others

nodeInput :: (Var -> T2) -> NodeCalc -> NodeCalc
nodeInput binding (IdCalc var (Out ds)) = IdCalc var (Out (binding var : ds))

refineInput :: (Var -> T2) -> (Ref, NodeCalc) -> Calculation -> Calculation
refineInput binding (ref, node) = transform $ insert ref (nodeInput binding node)

output :: Refinement -> [T2]
output (Refined (Calc root nodes)) = reverse ds
  where Out ds = nodeOutput (nodes!root)

rootOffset :: Integral i => Expr -> i
rootOffset x = offset (shifts x ! rootRef x)
