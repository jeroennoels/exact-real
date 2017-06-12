{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ternary.Recursive where

import Ternary.Core.Digit (T2(..))
import Ternary.Sampling.Expression (Binding)
import Ternary.Sampling.Calculation hiding (Depth, refine)

data Analysis = IncDepth | Bailout | Inconclusive
              deriving Show

class Refinable r s | r -> s, s -> r  where
  refine :: r -> Either r s
  proceed :: Binding T2 -> s -> Either r s

class Analyze r where
  analyze :: Depth -> r -> Analysis


newtype Walk = Walk [T2] deriving Show

cons :: T2 -> Walk -> Walk
cons a (Walk as) = Walk (a:as)

type Walk2 = (Walk, Walk)
type Acc = [Walk2]

data Ready = BeginLevel | NextLevel deriving Show
data Stalled = Stall deriving Show

-- BeginLevel --> Stall --> NextLevel --> BeginLevel ...

instance Refinable Ready Stalled where
  refine BeginLevel = Right Stall
  refine NextLevel = Left BeginLevel
  proceed _ Stall = Left NextLevel

instance Analyze Ready where
  analyze depth _ | depth > limit = Bailout  
  analyze _ BeginLevel = Inconclusive
  analyze _ NextLevel = IncDepth


type Depth = Int

limit :: Depth
limit = 2

recurse :: (Refinable r s, Analyze r) =>
           Walk2 -> Depth -> Either r s -> Acc -> Acc
-- left
recurse c depth (Left refinable) acc =
  case analyze depth refinable of
   Bailout -> c:acc
   IncDepth -> recurse c (depth+1) (refine refinable) acc 
   Inconclusive -> recurse c depth (refine refinable) acc
-- right
recurse c depth (Right stalled) acc =
  let go :: Acc -> (Walk2, Binding T2) -> Acc
      go accum (extended, binding) =
        recurse extended depth (proceed binding stalled) accum
  in foldl go acc (branching c)


prepare :: Walk2 -> T2 -> (Walk2, Binding T2)
prepare (a,b) d = ((cons d a, b), const d)

branching :: Walk2 -> [(Walk2, Binding T2)]
branching c = map (prepare c) [M1, O0, P1]

-- recurse (Walk [], Walk []) 1 (Left BeginLevel) []
