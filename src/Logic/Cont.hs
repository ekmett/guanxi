{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Logic.Cont where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fail as Fail
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.Foldable (fold)
import Data.Functor.Identity
import Logic.Class
import Unaligned.Base

newtype LogicT m a = LogicT
  { runLogicT :: forall r. (a -> m r -> m r) -> m r -> m r
  }

type Logic = LogicT Identity

runLogic :: Logic a -> forall r. (a -> r -> r) -> r -> r
runLogic l s f = runIdentity $ runLogicT l (fmap . s) (Identity f)

pattern Logic :: (forall r . (a -> r -> r) -> r -> r) -> Logic a
pattern Logic f <- (runLogic -> f)
  where Logic f = LogicT $ \ k -> Identity . f (\ a -> runIdentity . k a . Identity) . runIdentity

instance Functor (LogicT f) where
  fmap f lt = LogicT $ \sk fk -> runLogicT lt (sk . f) fk

instance Applicative (LogicT f) where
  pure a = LogicT $ \sk fk -> sk a fk
  f <*> a =
    LogicT $ \sk fk -> runLogicT f (\g fk' -> runLogicT a (sk . g) fk') fk

instance Alternative (LogicT f) where
  empty = LogicT $ \_ fk -> fk
  f1 <|> f2 = LogicT $ \sk fk -> runLogicT f1 sk (runLogicT f2 sk fk)

instance Monad (LogicT m) where
  return = pure
  m >>= f = LogicT $ \sk fk -> runLogicT m (\a fk' -> runLogicT (f a) sk fk') fk
#if __GLASGOW_HASKELL__ < 808
  fail _ = LogicT $ \_ fk -> fk
#endif

instance MonadFail (LogicT m) where
  fail _ = LogicT $ \_ fk -> fk

instance MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = LogicT $ \sk fk -> m >>= \a -> sk a fk

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit m = lift $ runLogicT m ssk (return Empty)
    where ssk a fk = return $ a :&: (lift fk >>= reflect)

instance (Monad m, Foldable m) => Foldable (LogicT m) where
  foldMap f m = fold $ runLogicT m (fmap . mappend . f) (return mempty)

instance Traversable (LogicT Identity) where
  traverse g l = runLogic l (\a ft -> c <$> g a <*> ft) (pure mzero)
    where c a l' = return a `mplus` l'

instance MonadReader r m => MonadReader r (LogicT m) where
  ask = lift ask
  local f m = LogicT $ \sk fk -> runLogicT m ((local f .) . sk) (local f fk)

instance MonadState s m => MonadState s (LogicT m) where
  get = lift get
  put = lift . put

instance MonadError e m => MonadError e (LogicT m) where
  throwError = lift . throwError
  catchError m h =
    LogicT $ \sk fk ->
      let handle r = r `catchError` \e -> runLogicT (h e) sk fk
       in handle $ runLogicT m (\a -> sk a . handle) fk

instance PrimMonad m => PrimMonad (LogicT m) where
  type PrimState (LogicT m) = PrimState m
  primitive f = lift (primitive f)

observe :: Logic a -> a
observe lt = runIdentity $ runLogicT lt (const . return) (error "No answer.")

observeAll :: Logic a -> [a]
observeAll = runIdentity . observeAllT

observeMany :: Int -> Logic a -> [a]
observeMany i = runIdentity . observeManyT i

observeT :: MonadFail m => LogicT m a -> m a
observeT lt = runLogicT lt (const . return) (Fail.fail "No answer.")

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = runLogicT m (fmap . (:)) (return [])

observeManyT :: Monad m => Int -> LogicT m a -> m [a]
observeManyT n m
  | n <= 0 = return []
  | n == 1 = runLogicT m (\a _ -> return [a]) (return [])
  | otherwise = runLogicT (msplit m) sk (return []) where
    sk Empty _ = return []
    sk (a :&: m') _ = (a :) `liftM` observeManyT (n - 1) m'

