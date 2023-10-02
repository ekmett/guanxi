{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Logic.ContUnwind where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fail as Fail
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Kind (Type)

import Logic.Class

import Unaligned.Base

-- | A variant on 'Logic.Cont.LogicT' that maintains a distinction between the
-- failure continuation and a monadic effect that must be executed when
-- backtracking.  In particular, this means that 'once' can discard the local
-- failure continuation without also making it impossible to backtrack.
type LogicT :: (Type -> Type) -> Type -> Type
newtype LogicT m a = LogicT
  { runLogicTWithUnwind :: forall r. (a -> m () -> m r -> m r) -> m () -> m r -> m r
  }

runLogicT :: Applicative m => LogicT m a -> (a -> m r -> m r) -> m r -> m r
runLogicT m sk fk = runLogicTWithUnwind m (\ a bk fk' -> sk a (bk *> fk')) (pure ()) fk

type Logic = LogicT Identity

runLogic :: Logic a -> forall r. (a -> r -> r) -> r -> r
runLogic l s f = runIdentity $ runLogicT l (fmap . s) (Identity f)

pattern Logic :: (forall r . (a -> r -> r) -> r -> r) -> Logic a
pattern Logic f <- (runLogic -> f)
  where Logic f = LogicT $ \ k _ -> Identity . f (\ a -> runIdentity . k a (Identity ()) . Identity) . runIdentity

instance Functor (LogicT f) where
  fmap f lt = LogicT $ \sk bk fk -> runLogicTWithUnwind lt (sk . f) bk fk

instance Applicative (LogicT f) where
  pure a = LogicT $ \sk bk fk -> sk a bk fk
  f <*> a =
    LogicT $ \sk bk fk -> runLogicTWithUnwind f (\g bk' fk' -> runLogicTWithUnwind a (sk . g) bk' fk') bk fk

instance Applicative f => Alternative (LogicT f) where
  empty = LogicT $ \ _ bk fk -> bk *> fk
  f1 <|> f2 = LogicT $ \sk bk fk -> runLogicTWithUnwind f1 sk (pure ()) (runLogicTWithUnwind f2 sk bk fk)
  -- The choice of backtracking continuations is subtle here   ^^^^^^^                 and here    ^^
  -- The point is that the incoming bk represents the effects that need to be
  -- executed if (f1 <|> f2) as a whole fails and needs to backtrack to an
  -- earlier choice point. Thus if f1 fails, we only backtrack as far as the
  -- choice point created by the call to <|>, so we try f2 without executing bk.
  -- If f2 fails as well, we then need to execute bk and backtrack further.

instance Monad (LogicT m) where
  return = pure
  m >>= f = LogicT $ \sk bk fk -> runLogicTWithUnwind m (\a bk' fk' -> runLogicTWithUnwind (f a) sk bk' fk') bk fk
#if __GLASGOW_HASKELL__ < 808
  fail _ = empty
#endif

instance Applicative m => MonadFail (LogicT m) where
  fail _ = empty

instance Applicative m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = LogicT $ \sk bk fk -> m >>= \a -> sk a bk fk

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where

  -- TODO: this definition could do with better justification.  In particular it
  -- is not wholly clear what should happen if the cleanup continuation itself
  -- backtracks.  Morally the cleanup continuation should only execute effects
  -- in the underlying monad, but we can't express that easily.
  pureWithCleanup (a :&&: bk) = LogicT $ \ sk bk' fk ->
      sk a (runLogicTWithUnwind bk (\ _ bk'' _ -> bk'') (pure ()) (pure ()) >> bk') fk

  cleanup v m = pureWithCleanup (() :&&: m) *> v -- TODO: really?

  msplit :: forall a. LogicT m a -> LogicT m (ViewWithCleanup a (LogicT m))
  msplit m = lift $ runLogicTWithUnwind m ssk (return ()) (return Empty)
    where
      ssk :: a -> m () -> m (ViewWithCleanup a (LogicT m)) -> m (ViewWithCleanup a (LogicT m))
      ssk a bk fk = return $ a :&&: lift bk :&: (lift fk >>= reflect)

instance (Monad m, Foldable m) => Foldable (LogicT m) where
  foldMap f m = fold $ runLogicT m (fmap . mappend . f) (return mempty)

instance Traversable (LogicT Identity) where
  traverse g l = runLogic l (\a ft -> c <$> g a <*> ft) (pure mzero)
    where c a l' = return a `mplus` l'

instance MonadReader r m => MonadReader r (LogicT m) where
  ask = lift ask
  local f m = LogicT $ \sk bk fk -> runLogicTWithUnwind m (\a bk' fk' -> local f (sk a bk' fk')) (local f bk) (local f fk)

instance MonadWriter w m => MonadWriter w (LogicT m) where
  tell = lift . tell
  pass = error "TODO: MonadWriter w (LogicT m) pass"
  listen = error "TODO: MonadWriter w (LogicT m) listen"

instance MonadState s m => MonadState s (LogicT m) where
  get = lift get
  put = lift . put

-- TODO: this instance typechecks but is not obviously correct.  The use of
-- 'handle' in two places with the same continuations is rather suspicious.
-- instance MonadError e m => MonadError e (LogicT m) where
--   throwError = lift . throwError
--   catchError m h =
--     LogicT $ \sk bk fk ->
--       let handle r = r `catchError` \e -> runLogicTWithUnwind (h e) sk bk fk
--        in handle $ runLogicTWithUnwind m (\a bk' -> sk a bk' . handle) bk fk -- TODO: which bk here?

instance PrimMonad m => PrimMonad (LogicT m) where
  type PrimState (LogicT m) = PrimState m
  primitive f = lift (primitive f)

observe :: Logic a -> a
observe lt = runIdentity $ runLogicT lt (const . return) (error "No answer.")

observeAll :: Logic a -> [a]
observeAll = runIdentity . observeAllT

observeT :: MonadFail m => LogicT m a -> m a
observeT lt = runLogicT lt (const . return) (Fail.fail "No answer.")

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = runLogicT m (fmap . (:)) (return [])
