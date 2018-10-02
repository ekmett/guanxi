{-# language LambdaCase #-}
{-# language DeriveTraversable #-}
{-# language ViewPatterns #-}

module Logic.Reflection where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Functor.Identity
import Logic.Class
import Unaligned

data L m a = Nil | Cons a (LogicT m a)
  deriving (Functor, Foldable, Traversable)

newtype LogicT m a = LogicT { runLogicT :: Cat (m (L m a)) }
  deriving (Functor, Foldable, Traversable)

type Logic = LogicT Identity

single :: Monad m => a -> m (L m a)
single a = return (Cons a empty)

unview :: m (L m a) -> LogicT m a
unview = LogicT . singleton

view :: Monad m => LogicT m a -> m (L m a)
view (LogicT s) = case uncons s of
   Empty -> return Nil
   h :&: t -> h >>= \case
     Nil -> view (LogicT t)
     Cons hi (LogicT ti) -> return $ Cons hi $ LogicT $ ti <> t

instance Monad m => Applicative (LogicT m) where
  pure = unview . single
  (<*>) = ap

instance Monad m => Alternative (LogicT m) where
  empty = LogicT mempty
  (view -> m) <|> n = unview $ m >>= return . \case
    Nil -> Nil
    Cons h t -> Cons h (LogicT $ runLogicT t <> runLogicT n) 

instance Monad m => Monad (LogicT m) where
  (view -> m) >>= f = unview $ m >>= \case
    Nil      -> return Nil
    Cons h t -> view $ f h <|> (t >>= f)
  fail _ = mzero

instance Monad m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)
 
instance MonadTrans LogicT where
  lift m = unview (m >>= single)

instance MonadIO m => MonadIO (LogicT m) where
    liftIO = lift . liftIO

simple :: L m a -> View a (LogicT m a)
simple Nil = Empty
simple (Cons a as) = a :&: as

instance Monad m => MonadLogic (LogicT m) where
  msplit (view -> m) = lift (simple <$> m)

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT (view -> m) = m >>= get where
  get (Cons a t) = liftM (a :) (observeAllT t)
  get _          = return []

observeT :: Monad m => LogicT m a -> m a
observeT (view -> m) = m >>= get where
  get (Cons a _) = return a
  get _          = fail "No results"
