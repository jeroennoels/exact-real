{-# LANGUAGE UnboxedTuples #-}

module Ternary.TestCompiler (compilerTest) where

import Control.Arrow (second)
import GHC.Int (Int16)

import Ternary.Core.Digit (T2(..), allT2)
import Ternary.Util.Misc (cross)
import Ternary.Compiler.ArrayLookup
import Ternary.Compiler.StateSpace (wrap, unwrap)
import Ternary.QuickCheckUtil (assert)

-- endomorphism with domain
data Endo a = Endo {dom :: [a], fun :: a -> a}

isIndentity :: Eq a => Endo a -> Bool
isIndentity endo = null $ filter wrong (dom endo)
  where wrong a = fun endo a /= a

compilerTest = putStrLn "\nCompiler unit tests:" >> a1 >> a2
  where
    a1 = assert "  splitIn versus MixIn:  " $
         isIndentity splitAndMixIn &&
         isIndentity mixAndSplitIn
    a2 = assert "  splitOut versus mixOut:" $
         isIndentity mixAndSplitOut

splitOutBoxed :: Int16 -> (T2, Int16)
splitOutBoxed i = let (#a,b#) = splitOut i in (a,b)

splitAndMixIn :: Endo Int16
splitAndMixIn = Endo {
  dom = [0..8926],
  fun = uncurry mixIn . second unwrap . splitIn . fromIntegral}

mixAndSplitIn :: Endo (T2, Int16)
mixAndSplitIn = Endo {
  dom = [M2,P2] `cross` [0..1539] ++ [M1,O0,P1] `cross` [0..1948],
  fun = second unwrap . splitIn . fromIntegral . uncurry mixIn}

mixAndSplitOut :: Endo (T2, Int16)
mixAndSplitOut = Endo {
  dom = allT2 `cross` [0..1948],
  fun = splitOutBoxed . mixOut . second (wrap . fromIntegral)}
