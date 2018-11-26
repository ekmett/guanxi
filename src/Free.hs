{-# language GADTs #-}
{-# language DeriveTraversable #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Free where

import Control.Arrow (Kleisli(..))
import Control.Monad (ap, liftM)
import Control.Category
import Prelude hiding ((.),id)

import Aligned

data Free f a where
  F :: FreeView f x -> Rev Cat (Kleisli (Free f)) x b -> Free f b

data FreeView f a = Pure a | Free (f (Free f a))
  deriving (Functor, Foldable, Traversable)

pattern V :: Functor f => FreeView f a -> Free f a
pattern V a <- (view -> a) where
  V b = unview b

view :: Functor f => Free f a -> FreeView f a
view (F h t) = case h of
  Pure a -> case unsnoc t of
    Empty     -> Pure a
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
