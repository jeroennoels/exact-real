The purpose of this exercise is to explore and implement [exact real
arithmetic](https://wiki.haskell.org/Exact_real_arithmetic).  Exact
real arithmetic combines arbitrary precision with [incremental
computation](https://en.wikipedia.org/wiki/Incremental_computing).

* The focus is on correctness and clarity.  Using Haskell we avoid all
  low-level distractions.
  
* I use a symmetric redundant ternary representation. So the radix is
  3 and the digit range is {-2,-1,0,1,2}.  I believe this is the sweet
  spot between binary and higher radix representations: radix 2 forces
  too much precision in the operands, whereas radix 4 is too complex.

* We capture the essence of each arithmetical operation in a state
  machine.  Each state transition typically produces one more digit of
  the result.  This is motivated by separation of concerns: a
  transition function is independent of the data structures used to
  represent a stream of digits.  The correctness of these state
  machines is therefor not coupled to the details of how to drive
  forward a computation.
  
* Efficiency is treated as a separate concern.  State machines
  described at a high level of abstraction can be compiled to an
  adequate low-level computational substrate.

In retrospect, I believe this approach was unnecessarily complicated.
But I learned a lot from it, about Haskell and about exact real
arithmetic.  Having the addition kernel magically produced by Z3 stood
in the way of gaining deeper understanding.