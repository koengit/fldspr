fldspr
======

Overview
--------

The fldspr language can be described as follows:

    L ::= par t < N: L
        | for t < N: L
        | L ; L  -- sequential composition
        | L || L -- parallel composition
        | x := e -- where x is a memory location and `e` is a pure expression
        | if c then L
        | alloc M in L

The general idea is to automatically transform programs written in fldspr
into the following:

    L0 ::= alloc M in L0
         | L1

    L1 ::= for i < N: L1
         | L1 ; L1
         | par t < N: L2

    L2 ::= for i < N: L2
         | L2 ; L2
         | if x then L2
         | x := e

Note that there is no nested parallelism here; this makes the language easily
compilable into something that'll run nicely on a GPU. Depending on the
backend, the code may then easily be translated into a single top-level
parallel for loop.


TODOs
-----

* Collect examples - programs we'd like to run well on this
* Be smarter about memory management - currently we just coalesce all
  memory into a single block which we allocate at startup
* Optimizations: CSE, basic arithmetic, theorem proving, let hoisting, etc.
* Pass arguments to programs
* CUDA backend
* Parallella backend
    * [Long-term] annotations for program layout
* MOAR BACKENDS
* Smarter handling of data dependencies - currently problems arise with
  un-nesting loops when the loop variable of the inner loop depends on the
  loop variable of the outer loop. Perhaps this restriction can be lifted with
  some smarts?
