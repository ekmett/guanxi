Modules
=======

This is a brief and incomplete description of the rough module layout of guanxi

* Aligned
  * Base - type-aligned sequences
  * Free - Reflection Without Remorse (RWR) free monad
  * Freer - RWR freer monad
* Cover
  * DLX - Knuth's dancling links
  * DXZ - DLX with ZDDs (zero-suppressed binary decision diagrams) https://aaai.org/ocs/index.php/AAAI/AAAI17/paper/view/14907
* Domain
  * Interval - interval arithmetic built with propagators
* FD (Finite Domain)
  * Monad - This monad drives everything else in guanxi for now
  * Var - finite domain variables encoded as sets of values
* Unaligned
  * Base - Okasaki-style catenable sequences and queues
* Logic
  * Class - LogicT type class
  * Cont - continuation-based implementation of LogicT. If you don't need reflection, this implementation is the fastest.
  * Naive - naive LogicT implementation
  * Reflection - RWR-style LogicT, using Unaligned.Base
* Prompt
  * Class - delimited continuations based on RWR
* Signal - partial propagator implementation
* Tactics - A toy tactic language
* Unique - Fast unique symbols
