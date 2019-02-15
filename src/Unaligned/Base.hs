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

module Unaligned.Base
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
  , pattern Nil
  , pattern Cons
  , pattern Snoc
  ) where

import Unaligned.Internal
