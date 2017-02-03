module Ternary.TestCompiler (compilerTest) where

import Control.Arrow (second)
import GHC.Int (Int16)

import Ternary.Core.Digit (T2(..), allT2)
import Ternary.Util.Misc (cross)
import Ternary.Compiler.ArrayLookup
import Ternary.Compiler.StateSpace

import Ternary.QuickCheckUtil (assert)

domainIn = [M2,P2] `cross` [0..1539] ++ [M1,O0,P1] `cross` [0..1948]

domainOut = allT2 `cross` [0..1948]

compilerTest = putStrLn "\nCompiler unit tests:" >> a1 >> a2
  where
    a1 = assert "  splitIn versus MixIn:  " $
         isIndentity splitAndMixIn [0..8926] &&
         isIndentity mixAndSplitIn domainIn
    a2 = assert "  splitOut versus mixOut:" $
         isIndentity mixAndSplitOut domainOut

splitAndMixIn :: Int16 -> Int16
splitAndMixIn = mixIn . second unwrap . splitIn . fromIntegral

mixAndSplitIn :: (T2, Int16) -> (T2, Int16)
mixAndSplitIn = second unwrap . splitIn . fromIntegral . mixIn

mixAndSplitOut :: (T2, Int16) -> (T2, Int16)
mixAndSplitOut = splitOut . mixOut . second (wrap . fromIntegral)

isIndentity :: Eq a => (a -> a) -> [a] -> Bool
isIndentity f = null . filter pred
  where pred a = f a /= a
