{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ternary.Recursive where

import Data.Functor.Compose
import Control.Applicative
import Ternary.Core.Digit (T2(..))
import Ternary.Sampling.Expression (Var(Var), Binding)
import Ternary.Sampling.Calculation hiding (Depth, refine, variables)

data Analysis = IncDepth | Bailout | Inconclusive
              deriving Show

class Refinable r s | r -> s, s -> r  where
  refine :: r -> Either r s
  proceed :: Binding T2 -> s -> Either r s
  variables :: s -> [Var]

class Analyze r where
  analyze :: Depth -> r -> Analysis


newtype Walk = Walk [T2] deriving Show

type Walk2 = (Walk, Walk)
type Acc = [Walk2]

data Ready = BeginLevel | NextLevel deriving Show
data Stalled = Stall deriving Show

-- BeginLevel --> Stall --> NextLevel --> BeginLevel ...

instance Refinable Ready Stalled where
  refine BeginLevel = Right Stall
  refine NextLevel = Left BeginLevel
  proceed _ Stall = Left NextLevel
  variables _ = [Var 0, Var 1]
  
instance Analyze Ready where
  analyze depth _ | depth > limit = Bailout  
  analyze _ BeginLevel = Inconclusive
  analyze _ NextLevel = IncDepth


type Depth = Int

limit :: Depth
limit = 8

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
  in foldl go acc (branching c (variables stalled))


cons :: Maybe T2 -> Walk -> Walk
cons Nothing w = w
cons (Just a) (Walk as) = Walk (a:as)


branching :: Walk2 -> [Var] -> [(Walk2, Binding T2)]
branching c@(a,b) vars = liftA2 (prepare c)
                   (branch vars (Var 0) a)
                   (branch vars (Var 1) b)

branch :: [Var] -> Var -> Walk -> [Maybe T2]
branch vars var _ | not (var `elem` vars) = [Nothing]
branch _ _ a | a `longerThan` 1 = [Just O0]
branch _ _ _ = map Just [M1,O0,P1]

longerThan (Walk xs) n = not $ null (drop n xs) 

prepare :: Walk2 -> Maybe T2 -> Maybe T2 -> (Walk2, Binding T2)
prepare (a,b) u v  = ((cons u a, cons v b), bind u v)

bind (Just u) _ (Var 0) = u
bind _ (Just v) (Var 1) = v
bind _ _ _ = error "Ternary.Recursive (bind)"

test = recurse (Walk [], Walk []) 1 (Left BeginLevel) []
