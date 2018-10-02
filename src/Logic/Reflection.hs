{-# language LambdaCase #-}
{-# language DeriveTraversable #-}
{-# language ViewPatterns #-}

module Logic.Reflection where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Identity
import Logic.Class
import Unaligned

type L m a = View a (LogicT m a)

newtype LogicT m a = LogicT { runLogicT :: Cat (m (L m a)) }

instance Functor m => Functor (LogicT m) where
  fmap f = LogicT . fmap (fmap (bimap f (fmap f))) . runLogicT

instance Foldable m => Foldable (LogicT m) where
  foldMap f = foldMap (foldMap (bifoldMap f (foldMap f))) . runLogicT

instance Traversable m => Traversable (LogicT m) where
  traverse f = fmap LogicT . traverse (traverse (bitraverse f (traverse f))) . runLogicT

type Logic = LogicT Identity

single :: Monad m => a -> m (L m a)
single a = return (a :&: empty)

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
  lift m = unview (m >>= single)

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit = lift . view

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = view m >>= go where
  go (a :&: t) = (a:) <$> observeAllT t
  go _ = return []

observeT :: Monad m => LogicT m a -> m a
observeT m = view m >>= go where
  go (a :&: _) = return a
  go _ = fail "No results"
