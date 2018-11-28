{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Unification.Meta where

import Aligned.Freer
import Control.Applicative
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.State.Class
import Data.Type.Coercion
import Data.Type.Equality
import Ref.Base
import Ref.Key
import Unification.Class

-- TODO: use the @name@ package and nominal unification? add rank? lambda-rank? 
newtype Meta u f = Meta { _metaRef :: Ref u (Maybe (Free f (Meta u f))) }
  deriving (Eq)

makeClassy ''Meta

instance Reference (Meta u f) u (Maybe (Free f (Meta u f))) where
  reference = _metaRef

instance TestEquality (Meta u) where
  testEquality (Meta u) (Meta v) = case testEquality u v of
    Just Refl -> Just Refl
    Nothing -> Nothing

instance TestCoercion (Meta u) where
  testCoercion (Meta u) (Meta v) = case testCoercion u v of
    Just Coercion -> Just Coercion
    Nothing -> Nothing

-- NB: readRef and writeRef work for Meta
newMeta :: (MonadKey m, MonadState s m, HasRefEnv s (KeyState m)) => m (Meta (KeyState m) f)
newMeta = Meta <$> newRef Nothing

-- TODO: move the rest to Aigned.Free.Unification and Aligned.Freer.Unification to avoid free vs. freer choice

unifyMeta :: (Alternative m, MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Unified f, Eq v) => v -> Free f v -> m (Free f v)
unifyMeta a x = readRef a >>= \case
  Nothing -> do
    x' <- zonk x
    guard $ not $ any (a ==) x'
    x' <$ writeRef a (Just x')
  Just y -> do
    y' <- unify x y
    y' <$ writeRef a (Just y')

unify :: (Alternative m, MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Unified f, Eq v) => Free f v -> Free f v -> m (Free f v)
unify l r = go l (view l) (view r) where
  go t (Pure v) (Pure u) | v == u = return t
  go _ (Pure a) y = unifyMeta a (unview y) -- TODO: union by rank in the (Pure a) (Pure b) case, requires ranked references, though
  go t _ (Pure a) = unifyMeta a t
  go _ (Free xs kx) (Free ys ky) =
    free <$> merge (\x y -> unify (kx x) (ky y)) xs ys

-- | zonk/walk-flatten
zonk :: (MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Traversable f) => Free f v -> m (Free f v)
zonk = fmap join . traverse go where
  go v = readRef v >>= \case
    Nothing -> pure $ pure v
    Just t -> do
      t' <- zonk t
      t' <$ writeRef v (Just t') -- path compression
