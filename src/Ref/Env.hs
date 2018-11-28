{-# language DeriveTraversable #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language ExplicitNamespaces #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language FunctionalDependencies #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Ref.Env
  ( Env
  , lookup
  , allocate
  , empty
  , size
  ) where

import Control.Lens
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude hiding (lookup)

data Env a = Env { _envMap :: HashMap Int a, size :: Int }
  deriving (Functor, Foldable, Traversable)

envMap :: Lens (Env a) (Env b) (HashMap Int a) (HashMap Int b)
envMap f (Env m s) = f m <&> \m' -> Env m' s

instance FoldableWithIndex Int Env where ifolded = envMap.itraversed
instance FunctorWithIndex Int Env where imapped = envMap.imapped
instance TraversableWithIndex Int Env where itraversed = envMap.itraversed

type instance Index (Env a) = Int
type instance IxValue (Env a) = a

instance Ixed (Env a)

instance At (Env a) where
  at i = envMap.at i

empty :: Env a
empty = Env mempty 0

allocate :: Int -> Env a -> (Int, Env a)
allocate i (Env m j) = (j, Env m (j+i))

lookup :: Int -> Env a -> Maybe a
lookup i (Env m _) = HashMap.lookup i m
