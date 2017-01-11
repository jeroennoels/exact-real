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
a = fromInteger 35780039053400943453990965313550011768798976735754765879085454657745645800390534009434539909653135500117687989767357547658790854546577654678976547543565656464565474655746576457645745765476547654764780039053400943453990965313550011768798976735754765879085454657780039053400943453990965313550011768798976735754765879085454657767554676586576585786858003905340094345399096531355001176879897673575476587908545465778003905340094345399096531355001176879897673575476587908545465778003905340094345399096531355001176879897673575476587908545465778003905340094345399096531355001176879897673575476587908545465778003905340094345399096531355001176879897673575476587908545465776547567588657800390534009434539909653135500117687989767357547658790854546577800390534009434539909653135500117687989767357547658790854546577800390534009434539909653135500117687989767357547658790854546577800390534009434539909653135500117687989767357547658790854546577546547655686578800390534009434539909653135500117687989767357547658790854546577800390534009434539909653135500117687989767357547658790854546577800390534009434539909653135500117687989767357547658790854546577

force :: Int -> Exact -> IO ()
force n x = streamDigits x !! n `seq` return ()

performance = timeMultiplication 700 a a

timeMultiplication :: Int -> Exact -> Exact -> IO ()
timeMultiplication n x y = 
  warmup
  >> force n x
  >> force n x
  >> time multiplyAltFS
  >> time multiplyAltIE
  where time (**) = timeIt $ force n (x ** y)
