{-# language LambdaCase #-}

module Logic.Naive where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Logic.Class
import Unaligned

type L m a = View a (LogicT m a)

newtype LogicT m a = LogicT { runLogicT :: m (L m a) }

instance Functor m => Functor (LogicT m) where
  fmap f = LogicT . fmap (bimap f (fmap f)) . runLogicT

instance Foldable m => Foldable (LogicT m) where
  foldMap f = foldMap (bifoldMap f (foldMap f)) . runLogicT

instance Traversable m => Traversable (LogicT m) where
  traverse f = fmap LogicT . traverse (bitraverse f (traverse f)) . runLogicT

single :: Monad m => a -> m (L m a)
single a = return (a :&: empty)

instance Monad m => Applicative (LogicT m) where
  pure = LogicT . single
  (<*>) = ap

instance Monad m => Monad (LogicT m) where
  LogicT m >>= f = LogicT $ m >>= \case
    Empty -> return Empty
    h :&: t -> runLogicT $ f h <|> (t >>= f)

instance Monad m => Alternative (LogicT m) where
  empty = LogicT $ return Empty
  LogicT a <|> b = LogicT $ a >>= \case
    Empty -> runLogicT b
    h :&: t -> pure $ h :&: (t <|> b)

instance Monad m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = LogicT (m >>= single)

instance Monad m => MonadLogic (LogicT m) where
  msplit (LogicT m) = lift m
