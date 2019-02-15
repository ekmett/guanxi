{-# language PatternSynonyms #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Catenable structures in the style of Purely Functional Data Structures
-- by Chris Okasaki
--
-- These vesions can be relocated by an offset, using the same techniques as found
-- <https://www.youtube.com/watch?v=090hIEiUoE0 Monoidal Parsing by Edward Kmett>

module Relative.Base
  ( View(..)
  , Cons(..)
  , Uncons(..)
  , Snoc(..)
  , Unsnoc(..)
  , Nil(..)
  , Singleton(..)
  , Q
  , Cat
  , Rev(..)
  , plus
  , pattern Nil
  , pattern Cons
  , pattern Snoc
  , Relative(..)
  , RelativeSemigroup
  , RelativeMonoid
  ) where

import Relative.Internal
