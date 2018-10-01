{-# language GADTs #-}

module Free where

import Control.Arrow (Kleisli(..))
import Control.Monad (ap, liftM)
import Control.Category
import Prelude hiding ((.),id)

import Cat

data Free f a where
  F :: FreeView f x -> Rev Cat (Kleisli (Free f)) x b -> Free f b

data FreeView f a = Pure a | Free (f (Free f a))

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

instance Applicative (Free f) where
  pure x = F (Pure x) id
  (<*>) = ap

instance Monad (Free f) where
  F m r >>= f = F m (cons (Kleisli f) r)
