{-# language LambdaCase #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Logic.Reflection
  ( LogicT (LogicT) -- TODO internal
  , Logic
  , observe, observeMany, observeAll
  , observeT, observeManyT, observeAllT
    -- TODO internal
  , view, unview
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Identity
import Logic.Class
import Unaligned.Base

type L m a = View a (LogicT m a)

type Logic = LogicT Identity

instance Nil (LogicT m) where
  nil = LogicT mempty

instance Monad m => Cons (LogicT m) where
  cons a xs = pure a <|> xs

instance Monad m => Snoc (LogicT m) where
  snoc xs a = xs <|> pure a

instance m ~ Identity => Uncons (LogicT m) where
  uncons = runIdentity . view

instance Monad m => Semigroup (LogicT m a) where
  (<>) = (<|>)
  -- LogicT xs <> LogicT ys = LogicT (xs <> ys)

instance Monad m => Monoid (LogicT m a) where
  mempty = LogicT mempty

newtype LogicT m a = LogicT { runLogicT :: Cat (m (L m a)) }

instance Functor m => Functor (LogicT m) where
  fmap f = LogicT . fmap (fmap (bimap f (fmap f))) . runLogicT

instance Foldable m => Foldable (LogicT m) where
  foldMap f = foldMap (foldMap (bifoldMap f (foldMap f))) . runLogicT

instance Traversable m => Traversable (LogicT m) where
  traverse f = fmap LogicT . traverse (traverse (bitraverse f (traverse f))) . runLogicT

single :: Monad m => a -> m (L m a)
single a = pure (a :&: empty)

unview :: m (L m a) -> LogicT m a
unview = LogicT . singleton

view :: Monad m => LogicT m a -> m (L m a)
view (LogicT s) = case uncons s of
  Empty -> return Empty
  h :&: t -> h >>= \case
    Empty -> view (LogicT t)
    hi :&: LogicT ti -> return $ hi :&: LogicT (ti <> t)

instance Monad m => Applicative (LogicT m) where
  pure = unview . single
  (<*>) = ap

instance Monad m => Alternative (LogicT m) where
  empty = LogicT mempty
  -- is this the best version?
  m <|> LogicT n = unview $
    fmap (\(LogicT t) -> LogicT $ t <> n) <$> view m

instance Monad m => Monad (LogicT m) where
  m >>= f = unview $ view m >>= \case
    Empty -> return Empty
    h :&: t -> view $ f h <|> (t >>= f)
  fail _ = mzero

instance Monad m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = unview $ m >>= single

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit = lift . view

instance PrimMonad m => PrimMonad (LogicT m) where
  type PrimState (LogicT m) = PrimState m
  primitive f = lift $ primitive f

observe :: Logic a -> a
observe = runIdentity . observeT

observeMany :: Int -> Logic a -> [a]
observeMany n = runIdentity . observeManyT n

observeAll :: Logic a -> [a]
observeAll m = go (runIdentity (view m)) where
  go :: forall a. View a (Logic a) -> [a]
  go (a :&: t) = a : observeAll t
  go _ = []

observeT :: Monad m => LogicT m a -> m a
observeT m = view m >>= go where
  go (a :&: _) = return a
  go _ = fail "No results"

observeManyT :: Monad m => Int -> LogicT m a -> m [a]
observeManyT n m
  | n <= 0 = return []
  | otherwise = view m >>= \case
    Empty -> return []
    a :&: m1 -> (a:) <$> observeManyT (n-1) m1

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = view m >>= go where
  go (a :&: t) = (a:) <$> observeAllT t
  go _ = return []
