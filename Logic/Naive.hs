{-# language DeriveTraversable #-}

module Logic.Reflection where

newtype L m a = Nil | Cons a (LogicT m a)
  deriving (Functor, Foldable, Traversable)

newtype LogicT m a = LogicT { runLogicT :: m (L m a) }
  deriving (Functor, Foldable, Traversable)

single :: a -> m (Maybe (a, LogicT m a))
single a = return (Cons a empty)

instance Monad m => Applicative (LogicT m) where
  pure a = LogicT . single
  (<*>) = ap

instance Monad m => Monad (LogicT m) where
  LogicT m >>= f = LogicT $ m >>= \case
    Nil -> return Nothing
    Cons h t -> runLogicT $ f h <|> (t >>= f)

instance Monad m => Alternative (LogicT m) where
  empty = LogicT $ return Nothing
  LogicT a <|> b = LogicT $ a >>= \case
    Nil -> runLogicT b
    Cons h t -> pure $ Cons h (mplus t b)

instance MonadTrans LogicT
  lift m = ML (m >>= single)

instance Monad m => MonadLogic (LogicT m) where
  msplit (LogicT m) = lift m


