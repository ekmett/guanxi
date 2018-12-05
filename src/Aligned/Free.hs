{-# language GADTs #-}
{-# language DeriveTraversable #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Aligned.Free where

import Aligned.Base
import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Monad (ap, liftM, guard, join)
import Control.Monad.State.Class
import Control.Category
import Prelude hiding ((.),id)
import Ref.Base
import Unification.Class


data Free f a where
  F :: FreeView f x -> Rev Cat (Kleisli (Free f)) x b -> Free f b

data FreeView f a = Pure a | Free (f (Free f a))
  deriving (Functor, Foldable, Traversable)

pattern V :: Functor f => FreeView f a -> Free f a
pattern V a <- (view -> a) where
  V b = unview b

view :: Functor f => Free f a -> FreeView f a
view (F h t) = case h of
  pa@(Pure a) -> case unsnoc t of
    Empty     -> pa
    tc :&: hc -> view $ runKleisli hc a ^>>= tc
  Free f -> Free $ fmap (^>>= t) f

unview :: FreeView f a -> Free f a
unview x = F x id

free :: f (Free f a) -> Free f a
free fx = F (Free fx) id

(^>>=) :: Free f x -> Rev Cat (Kleisli (Free f)) x b -> Free f b
F h t ^>>= r = F h (r . t)

instance Functor (Free f) where
  fmap = liftM

instance (Functor f, Foldable f) => Foldable (Free f) where
  foldMap f = foldMap f . view

instance Traversable f => Traversable (Free f) where
  traverse f = fmap unview . traverse f . view

instance Applicative (Free f) where
  pure x = F (Pure x) id
  (<*>) = ap

instance Monad (Free f) where
  F m r >>= f = F m (cons (Kleisli f) r)

unifyMeta :: (Alternative m, MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Unified f, Eq v) => v -> Free f v -> m (Free f v)
unifyMeta a x = readRef a >>= \case
  Nothing -> do
    x' <- zonk x
    guard $ notElem a x'
    x' <$ writeRef a (Just x')
  Just y -> do
    y' <- unify x y
    y' <$ writeRef a (Just y')

unify :: (Alternative m, MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Unified f, Eq v) => Free f v -> Free f v -> m (Free f v)
unify l r = go l (view l) (view r) where
  go t (Pure v) (Pure u) | v == u = return t
  go _ (Pure a) y = unifyMeta a (unview y) -- TODO: union by rank in the (Pure a) (Pure b) case, requires ranked references, though
  go t _ (Pure a) = unifyMeta a t
  go _ (Free xs) (Free ys) =
    free <$> merge unify xs ys

-- | zonk/walk-flatten
zonk :: (MonadState s m, HasRefEnv s u, Reference v u (Maybe (Free f v)), Traversable f) => Free f v -> m (Free f v)
zonk = fmap join . traverse go where
  go v = readRef v >>= \case
    Nothing -> pure $ pure v
    Just t -> do
      t' <- zonk t
      t' <$ writeRef v (Just t') -- path compression

