{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ternary.Recursive where

import Control.Applicative (liftA2)
import Ternary.Core.Digit (T2(..))
import Ternary.Sampling.Expression (Var(Var), Binding)


data Analysis = IncDepth | Bailout | Inconclusive
              deriving Show

class Refinable r s | r -> s, s -> r  where
  refine :: r -> Depth -> Either r s
  proceed :: Binding T2 -> s -> Either r s
  variables :: s -> [Var]

class Analyze r where
  analyze :: Depth -> r -> Analysis


data Walk = Walk [T2] Int deriving Show

type Walk2 = (Walk, Walk)
type Acc = [(Walk2, Depth)]

data Ready = BeginLevel | NextLevel deriving Show
data Stalled = Stall Int deriving Show

-- BeginLevel --> Stall --> NextLevel --> BeginLevel ...

instance Refinable Ready Stalled where
  refine BeginLevel depth = Right (Stall depth)
  refine NextLevel _ = Left BeginLevel
  proceed _ (Stall n) = Left NextLevel
  variables (Stall n) = case n `rem` 4 of
    0 -> [Var 0, Var 1]
    1 -> [Var 1]
    otherwise -> [Var 0]
  
instance Analyze Ready where
  analyze depth _ | depth > limit = Bailout  
  analyze _ BeginLevel = Inconclusive
  analyze _ NextLevel = IncDepth


type Depth = Int

limit :: Depth
limit = 7
logres = 1

recurse :: (Refinable r s, Analyze r) =>
           Walk2 -> Depth -> Either r s -> Acc -> Acc
-- left
recurse c depth (Left refinable) acc =
  case analyze depth refinable of
   Bailout -> (c,depth):acc
   IncDepth -> recurse c (depth+1) (refine refinable depth) acc 
   Inconclusive -> recurse c depth (refine refinable depth) acc
-- right
recurse c depth (Right stalled) acc =
  let go :: Acc -> (Walk2, Binding T2) -> Acc
      go accum (extended, binding) =
        recurse extended depth (proceed binding stalled) accum
  in foldl go acc (branching c (variables stalled))

done (Walk xs _) = not $ null (drop logres xs) 

cons :: Maybe T2 -> Walk -> Walk
cons _ w@(Walk as n) | done w = Walk as (n+1)
cons (Just a) (Walk as 0) = Walk (a:as) 0
cons Nothing w = w

branching :: Walk2 -> [Var] -> [(Walk2, Binding T2)]
branching c@(a,b) vars = liftA2 (prepare c)
                   (branch vars (Var 0) a)
                   (branch vars (Var 1) b)

branch :: [Var] -> Var -> Walk -> [Maybe T2]
branch vars var _ | var `notElem` vars = [Nothing]
branch _ _ a | done a = [Just O0]
branch _ _ _ = map Just [M1,O0,P1]

prepare :: Walk2 -> Maybe T2 -> Maybe T2 -> (Walk2, Binding T2)
prepare (a,b) u v  = ((cons u a, cons v b), bind u v)

bind (Just u) _ (Var 0) = u
bind _ (Just v) (Var 1) = v
bind _ _ _ = error "Ternary.Recursive (bind)"

test = recurse (Walk [] 0, Walk [] 0) 1 (Left BeginLevel) []

