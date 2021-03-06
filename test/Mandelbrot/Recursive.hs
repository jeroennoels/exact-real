{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- We model the recursive branching strategy as a separate concern.

module Mandelbrot.Recursive where

import Control.Applicative (liftA2)
import Ternary.Core.Digit (T2(..))
import Ternary.Sampling.Expression (Var(Var), Binding)

type Depth = Int

data Analysis = IncDepth | Bailout | Inconclusive
              deriving Show

class Refinable r s | r -> s, s -> r  where
  refine :: r -> Depth -> Either s r
  proceed :: Binding T2 -> s -> Either s r
  variables :: s -> [Var]

class Analyze r where
  analyze :: Depth -> r -> Analysis


data Walk = After [T2] Int | Branching [T2] Int
          deriving Show

type Walk2 = (Walk, Walk)
type Acc = [(Walk2, Depth)]

-- The following type are just for unit testing the recursion.

data Ready = BeginLevel | NextOdd | NextEven deriving Show
data Stalled = Stall Int deriving Show

-- BeginLevel --> Stall --> Next[Odd|Even] --> BeginLevel

instance Refinable Ready Stalled where
  refine BeginLevel depth = Left (Stall depth)
  refine NextOdd _ = Right BeginLevel
  refine NextEven _ = Right BeginLevel
  proceed _ (Stall n) = Right $ if odd n then NextOdd else NextEven
  variables (Stall n) = case n `rem` 4 of
    0 -> [Var 0, Var 1]
    1 -> [Var 1]
    otherwise -> [Var 0]

instance Analyze Ready where
  analyze depth _ | depth > limit = Bailout where limit = 5
  analyze depth NextOdd | odd depth = IncDepth
  analyze depth NextEven | even depth = IncDepth
  analyze _ _ = Inconclusive


recurse :: (Refinable r s, Analyze r) =>
           Walk2 -> Depth -> Either s r -> Acc -> Acc
-- Analyze the current state. Bail out or increase depth if needed.
recurse c depth rr@(Right refinable) acc =
  case analyze depth refinable of
   Bailout -> (c,depth):acc
   IncDepth -> recurse c (depth+1) rr acc
   Inconclusive -> recurse c depth (refine refinable depth) acc
-- Provide new input via branching.
recurse c depth (Left stalled) acc =
  let go :: Acc -> (Walk2, Binding T2) -> Acc
      go accum (extended, binding) =
        recurse extended depth (proceed binding stalled) accum
  in foldl go acc (branching c (variables stalled))

      
path :: Walk -> [T2]
path (After xs _) = reverse xs

cons :: Maybe T2 -> Walk -> Walk
cons _ w@(After as n) = After as (n+1)
cons (Just a) (Branching as n) | n > 0 = Branching (a:as) (n-1)
cons _ (Branching as 0) = After as 0
cons Nothing w = w

branching :: Walk2 -> [Var] -> [(Walk2, Binding T2)]
branching c@(a,b) vars =
  let (##) = liftA2 (cons2 c)
  in branch vars (Var 0) a ## branch vars (Var 1) b


branch :: [Var] -> Var -> Walk -> [Maybe T2]
branch vars var _ | var `notElem` vars = [Nothing]
branch _ _ (Branching _ 0) = [Just O0]
branch _ _ (After _ _) = [Just O0]
branch _ _ _ = map Just [M1,O0,P1]

cons2 :: Walk2 -> Maybe T2 -> Maybe T2 -> (Walk2, Binding T2)
cons2 (a,b) u v  = ((cons u a, cons v b), bind u v)

bind (Just u) _ (Var 0) = u
bind _ (Just v) (Var 1) = v
bind _ _ _ = error "Ternary.Recursive (bind)"

test = recurse (Branching [] 3, Branching [] 3) 1 (Right BeginLevel) []
