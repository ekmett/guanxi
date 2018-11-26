{-# language PatternSynonyms #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Type-aligned sequences in the style of Atze van der Ploeg's 
-- <http://okmij.org/ftp/Haskell/zseq.pdf Reflection without Remorse>

module Aligned
  ( View(..)
  , Cons(..)
  , Uncons(..)
  , Snoc(..)
  , Unsnoc(..)
  , Nil(..)
  , Op(..)
  , Thrist
  , Q
  , Cat
  , Rev(..)
  , pattern Cons
  , pattern Snoc
  , pattern Nil
  ) where

import Internal.Aligned
