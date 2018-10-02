{-# language DeriveTraversable #-}
{-# language LambdaCase #-}

module Logic.Naive where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Logic.Class
import Unaligned

data L m a = Nil | Cons a (LogicT m a)
  deriving (Functor, Foldable, Traversable)

newtype LogicT m a = LogicT { runLogicT :: m (L m a) }
  deriving (Functor, Foldable, Traversable)

single :: Monad m => a -> m (L m a)
single a = return (Cons a empty)

instance Monad m => Applicative (LogicT m) where
  pure = LogicT . single
  (<*>) = ap

instance Monad m => Monad (LogicT m) where
  LogicT m >>= f = LogicT $ m >>= \case
    Nil -> return Nil
    Cons h t -> runLogicT $ f h <|> (t >>= f)

instance Monad m => Alternative (LogicT m) where
  empty = LogicT $ return Nil
  LogicT a <|> b = LogicT $ a >>= \case
    Nil -> runLogicT b
    Cons h t -> pure $ Cons h (t <|> b)

instance Monad m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = LogicT (m >>= single)

simple :: L m a -> View a (LogicT m a)
simple Nil = Empty
simple (Cons a as) = a :&: as

instance Monad m => MonadLogic (LogicT m) where
  msplit (LogicT m) = lift (simple <$> m)


