module Ternary.TestCompiler (compilerTest) where

import Control.Arrow (second)
import GHC.Int (Int16)

import Ternary.Core.Digit (T2(..), allT2)
import Ternary.Util.Misc (cross)
import Ternary.Compiler.ArrayLookup
import Ternary.Compiler.StateSpace
import Ternary.QuickCheckUtil (assert)

-- endomorphism with domain
data Endo a = Endo {dom :: [a], fun :: a -> a}

isIndentity :: Eq a => Endo a -> Bool
isIndentity endo = null $ filter pred (dom endo)
  where pred a = fun endo a /= a

compilerTest = putStrLn "\nCompiler unit tests:" >> a1 >> a2
  where
    a1 = assert "  splitIn versus MixIn:  " $
         isIndentity splitAndMixIn &&
         isIndentity mixAndSplitIn
    a2 = assert "  splitOut versus mixOut:" $
         isIndentity mixAndSplitOut

splitAndMixIn :: Endo Int16
splitAndMixIn = Endo {
  dom = [0..8926],
  fun = mixIn . second unwrap . splitIn . fromIntegral}

mixAndSplitIn :: Endo (T2, Int16)
mixAndSplitIn = Endo {
  dom = [M2,P2] `cross` [0..1539] ++ [M1,O0,P1] `cross` [0..1948],
  fun = second unwrap . splitIn . fromIntegral . mixIn}

mixAndSplitOut :: Endo (T2, Int16)
mixAndSplitOut = Endo {
  dom = allT2 `cross` [0..1948],
  fun = splitOut . mixOut . second (wrap . fromIntegral)}
