module Domain.Interval
  ( Interval, Z
  , abstract, concrete
  , refine
  , interval
  , bottom
  , signumi
  , negatei
  , absi
  -- relations
  , lt,  le,  eq,  ne,  ge,  gt
  , zlt, zle, zeq, zne, zge, zgt
  , ltz, lez, eqz, nez, gez, gtz
  , onceBoundedBelow, onceBoundedAbove
  , onceKnown
  -- , poly
  , onHi, onLo
  , deltaHi, deltaLo
  , known
  ) where

import Domain.Internal
