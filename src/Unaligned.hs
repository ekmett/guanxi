{-# language PatternSynonyms #-}

module Unaligned
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

import Internal.Unaligned
