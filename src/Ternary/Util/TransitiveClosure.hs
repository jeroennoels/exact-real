{-# LANGUAGE ScopedTypeVariables #-}

module Ternary.Util.TransitiveClosure (reachTransitively) where

import Data.Maybe (fromJust)
import Data.Set (Set, unions, union, difference)
import qualified Data.Set as Set


collectSuccess :: Ord a => Set (Maybe a) -> Set a
collectSuccess as = Set.map fromJust $ Set.delete Nothing as

-- elements that can be reached in one step

reach :: forall a b . Ord b => Set a -> [a -> Maybe b] -> Set b
reach from = unions . map range
  where
    range :: (a -> Maybe b) -> Set b
    range f = collectSuccess $ Set.map f from

-- elements that can be reached recursively

reachTransitively :: forall a . Ord a => [a -> Maybe a] -> Set a -> Set a
reachTransitively fs from = fst $ grow (from,from)
  where
    grow :: (Set a, Set a) -> (Set a, Set a)
    grow pair@(acc,previous)
      | Set.null previous = pair
      | otherwise = let next = reach previous fs `difference` acc
                    in grow (acc `union` next, next)
