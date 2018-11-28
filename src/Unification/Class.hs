{-# language DefaultSignatures #-}
{-# language TypeOperators #-}
{-# language EmptyCase #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language BangPatterns #-}
{-# language RankNTypes #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Unification.Class
  ( Unified(..)
  , GUnified
  ) where

import Aligned.Freer
import Control.Applicative
import GHC.Generics
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Compose
import Data.Proxy

class Traversable f => Unified f where
  merge :: Alternative t => (a -> b -> t c) -> f a -> f b -> t (f c)
  default merge :: (Generic1 f, GUnified (Rep1 f), Alternative t)
                => (a -> b -> t c) -> f a -> f b -> t (f c)
  merge f l r = to1 <$> gmerge f (from1 l) (from1 r)

instance (Unified f, Unified g) => Unified (Sum f g)
instance (Unified f, Unified g) => Unified (Compose f g) where
  merge f (Compose l) (Compose r) = Compose <$> merge (merge f) l r

instance (Unified f, Unified g) => Unified (Product f g)
instance Unified []
instance Unified Maybe
instance Unified Proxy
instance Eq e => Unified (Const e)
instance Eq e => Unified ((,) e)
instance Unified Identity

-- using RwoR to force layer by layer
instance Unified f => Unified (Free f) where
  merge f l r = go (view l) (view r) where
    --go :: forall (f :: Type -> Type).  Unified f => FreeView f a -> FreeView f b -> t (Free f c)
    go (Pure a) (Pure b)   = pure <$> f a b
    go (Free as ka) (Free bs kb) = free <$> merge (\a b -> merge f (ka a) (kb b)) as bs
    go _ _ = empty

class GUnified f where
  gmerge :: Alternative t => (a -> b -> t c) -> f a -> f b -> t (f c)

instance GUnified p => GUnified (M1 i c p) where
  gmerge f (M1 a) (M1 b) = M1 <$> gmerge f a b

instance Eq c => GUnified (K1 i c) where
  gmerge _ (K1 a) (K1 b)
    | a == b = pure $ K1 a
    | otherwise = empty

instance GUnified U1 where
  gmerge _ _ _ = pure U1

instance GUnified V1 where
  gmerge _ !v _ = case v of {}

instance (GUnified f, GUnified g) => GUnified (f :*: g) where
  gmerge f (a :*: b) (c :*: d) = (:*:) <$> gmerge f a c <*> gmerge f b d

instance (GUnified f, GUnified g) => GUnified (f :+: g) where
  gmerge f (L1 l) (L1 r) = L1 <$> gmerge f l r 
  gmerge f (R1 l) (R1 r) = R1 <$> gmerge f l r 
  gmerge _ _ _ = empty

instance (Unified f, GUnified g) => GUnified (f :.: g) where
  gmerge f (Comp1 l) (Comp1 r) = Comp1 <$> merge (gmerge f) l r

instance GUnified Par1 where
  gmerge f (Par1 a) (Par1 b) = Par1 <$> f a b

instance Unified f => GUnified (Rec1 f) where
  gmerge f (Rec1 a) (Rec1 b) = Rec1 <$> merge f a b
