module Ternary.Core.Addition (Sa(Sa0), plus) where

import Ternary.Core.Kernel (Kernel)
import Ternary.Core.Digit (T2(..), T4(..))

-- The addition kernel needs only three states.

data Sa = Sa0 | Sa1 | Sa2 deriving (Show, Eq, Ord)

-- The following I found using the Z3 solver!

plus :: Kernel T4 T2 Sa
plus Ma1 Sa1 = (P1, Sa2)
plus Ma1 Sa2 = (M1, Sa2)
plus Ma2 Sa0 = (M1, Sa1)
plus Ma2 Sa1 = (O0, Sa1)
plus Ma2 Sa2 = (M2, Sa1)
plus Ma3 Sa0 = (M1, Sa0)
plus Ma3 Sa1 = (O0, Sa0)
plus Ma3 Sa2 = (M2, Sa0)
plus Ma4 Sa0 = (M1, Sa2)
plus Ma4 Sa2 = (M2, Sa2)
plus Oa0 Sa0 = (O0, Sa0)
plus Oa0 Sa1 = (P1, Sa0)
plus Oa0 Sa2 = (M1, Sa0)
plus Pa1 Sa0 = (O0, Sa1)
plus Pa1 Sa1 = (P1, Sa1)
plus Pa1 Sa2 = (M1, Sa1)
plus Pa2 Sa0 = (P1, Sa2)
plus Pa2 Sa1 = (P2, Sa2)
plus Pa3 Sa0 = (P1, Sa0)
plus Pa3 Sa1 = (P2, Sa0)
plus Pa3 Sa2 = (O0, Sa0)
plus Pa4 Sa0 = (P1, Sa1)
plus Pa4 Sa1 = (P2, Sa1)
plus Pa4 Sa2 = (O0, Sa1)
plus _ _ = (O0, Sa2)
