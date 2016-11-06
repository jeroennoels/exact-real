The purpose of this project is to study and implement [exact real
arithmetic](https://wiki.haskell.org/Exact_real_arithmetic).  Exact
real arithmetic combines arbitrary precision with [incremental
computation](https://en.wikipedia.org/wiki/Incremental_computing).

Project characteristics and goals

* My focus is on correctness and clarity.  Using haskell we avoid all
  low-level distractions.  Initially I rely on tests to explore the
  subject.  But I also want to pave the road towards formal
  verification with Coq.
  
* I use a symmetric redundant ternary representation. So the radix is
  3 and the digit range is {-2,-1,0,1,2}.  I believe this is the sweet
  spot between binary and higher radix representations: radix 2 forces
  too much precision in the operands, whereas radix 4 is too complex.

* I want type safety. All the way down to the digit.

* We capture the essence of each arithmetical operation in a state
  machine.  Each state transition typically produces one more digit of
  the result.  This is motivated by separation of concerns: a
  transition function is independent of the data structures used to
  represent a stream of digits.  The correctness of these state
  machines is therefor not coupled to the details of how to drive
  forward a computation.

* I believe modern [power tools](https://github.com/Z3Prover/z3) can
  bring new solutions to these ancient problems.
  
* Efficiency is treated as a separate concern.  State machines
  described at a high level of abstraction can be compiled to an
  adequate low-level computational substrate.
