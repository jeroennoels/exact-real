module Ternary.Core.Kernel (
  Kernel, FirstTwoSteps (Step0, Step1),
  serial, chain, unsafeIgnoreInput,
  zipKernelsWith, transformFirstTwo) where

-- A kernel is a machine with an internal state.  This state is such a
-- fundamental type here, that I decided not to hide it.

type Kernel input output state = input -> state -> (output, state)

-- Sequential composition

serial :: Kernel a b s -> Kernel b c t -> Kernel a c (s,t)
serial f g a (s,t) = let (b,u) = f a s
                         (c,v) = g b t
                     in (c,(u,v))

-- Parallel composition

zipKernelsWith ::
  (b -> d -> z) -> Kernel a b s -> Kernel c d t -> Kernel (a,c) z (s,t)
zipKernelsWith op f g (a,c) (s,t) = f a s `op1` g c t
  where (b,s) `op1` (d,t) = (b `op` d, (s,t))


-- We need a state machine that transforms its input only during the
-- first two cycles.

data FirstTwoSteps = Step0 | Step1 | After
                   deriving (Show, Eq, Ord)

transformFirstTwo :: (a -> a) -> (a -> a) -> Kernel a a FirstTwoSteps
transformFirstTwo f _ a Step0 = (f a, Step1)
transformFirstTwo _ g a Step1 = (g a, After)
transformFirstTwo _ _ a After = (a, After)

-- With the chain function below, we define sequential composition of
-- a list of kernels, combining states into a list of states.

-- The first argument generates a kernel from a parameter.  So a list
-- of such parameters implicitly defines the list of kernels to be
-- chained.  But this list of kernels is not materialized: they are
-- applied as we stream through the list of parameters.

-- The type of the chain function could be expressed more accurately
-- with dependent types, or using Data.FixedList.  I could formulate
-- this as a fold, but I doubt that it would be clearer.

chain :: (p -> Kernel a a s) -> [p] -> Kernel a a [s]
chain gen (p:ps) a (u:us) =
  let (b,v) = gen p a u
      (c,vs) = b `seq` chain gen ps b us
  in (c,v:vs)
chain _ [] a [] = (a,[])

-- For a given state, a lazy kernel may or may not force its input.
-- When we implement multiplication, we shall encounter the following
-- situation: we want a particular machine to make a state transition,
-- but we have no meaningful way to select an input value.  Moreover
-- we know that the input is irrelevant for this kernel in that state.
-- I do not want to choose an arbitrary value for the input.  For the
-- moment I prefer to see an error when my assumptions are invalid.

unsafeIgnoreInput :: Kernel a b s -> s -> (b,s)
unsafeIgnoreInput f s = f ignore s
  where ignore = error "Kernel (unsafeIgnoreInput)"
