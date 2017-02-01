module Ternary.TestCompiler where

import Control.Arrow (second)
import GHC.Int (Int16)

import Ternary.Core.Digit (T2, allT2)
import Ternary.Util.Misc (cross)
import Ternary.Compiler.ArrayLookup
import Ternary.Compiler.StateSpace

import Ternary.QuickCheckUtil (assert)

testCompiler = a1 >> a2  where
  a1 = assert "Compiler: splitIn versus MixIn" $
       isIndentity splitAndMixIn [0..8926]
  a2 = assert "Compiler: splitOut versus mixOut" $
       isIndentity mixAndSplitOut $ allT2 `cross` [0..1948]

splitAndMixIn :: Int16 -> Int16
splitAndMixIn = mixIn . second unwrap . splitIn . fromIntegral

mixAndSplitOut :: (T2, Int16) -> (T2, Int16)
mixAndSplitOut = splitOut . mixOut . second (wrap . fromIntegral)

isIndentity :: Eq a => (a -> a) -> [a] -> Bool
isIndentity f = null . filter pred
  where pred a = f a /= a

