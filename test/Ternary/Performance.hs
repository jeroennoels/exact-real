module Ternary.Performance where

import System.TimeIt

import Ternary.List.Exact
import Ternary.List.ExactNum
import Ternary.Compiler.StateSpace (integerEncoding)
import qualified Ternary.Compiler.StateSpace as Compiler (warmup)

warmup :: IO ()
warmup = if Compiler.warmup then putStrLn "warmup" else error "warmup"

-- arbitrary example
a :: Exact
a = fromInteger 357800390534009434539909653135500117687989767357547658790854546577

force :: Int -> Exact -> IO ()
force n x = streamDigits x !! n `seq` return ()

performance = timeMultiplication 200 a a

timeMultiplication :: Int -> Exact -> Exact -> IO ()
timeMultiplication n x y = 
  warmup
  >> force n x
  >> force n x
  >> time multiplyAltFS
  >> time multiplyAltIE
  where time (**) = timeIt $ force n (x ** y)
