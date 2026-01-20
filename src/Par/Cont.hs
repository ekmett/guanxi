{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Par.Cont
  ( ParEnv(..), HasParEnv(..)
  , Par(Par), runPar
  , statePar
  , parState
  ) where

#if MIN_VERSION_base(4,13,0)
import Control.Monad
#else
import Control.Monad hiding (fail)
#endif
#if MIN_VERSION_mtl(2,3,1)
import Control.Monad.Cont
#else
import Control.Monad.Cont hiding (fail) -- pre-2.3.1 re-exported Control.Monad fail
#endif
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.Primitive
import Control.Monad.Reader.Class
#if MIN_VERSION_mtl(2,3,1)
import Control.Monad.State.Strict
#else
import Control.Monad.State.Strict hiding (fail) -- pre-2.3.1 re-exported Control.Monad fail
#endif
import Control.Applicative
import Control.Lens hiding (Empty, snoc, uncons)
import Data.Default
import Logic.Class
import Par.Class
#if MIN_VERSION_base(4,13,0)
import Prelude
#else
import Prelude hiding (fail)
#endif
import Ref
import Unaligned.Base

type Task m = ParEnv m -> m (ParEnv m)

-- TODO: can we just store this in a backtracked reference?
newtype ParEnv m = ParEnv { _todo :: Q (Task m) }

instance Default (ParEnv m) where
  def = ParEnv def

makeClassy ''ParEnv

statePar :: (Alternative m, MonadRef m) => Par m a -> StateT (ParEnv m) m a
statePar (Par m) = StateT $ \s -> do
  r <- newRef Nothing
  s' <- m (\ a s' -> s' <$ writeRef r (Just a)) s
  readRef r >>= \case
    Nothing -> empty
    Just a -> pure (a, s')

newtype Par m a = Par
  { runPar :: (a -> Task m) -> Task m
  } deriving Functor

parState :: Monad m => StateT (ParEnv m) m a -> Par m a
parState (StateT m) = Par $ \k s -> do
  (a,s') <- m s
  k a s'

instance Applicative (Par m) where
  pure x  = Par $ \k -> k x
  Par f <*> Par v = Par $ \ c -> f $ \ g -> v (c . g)

instance Monad (Par m) where
  Par m >>= k = Par $ \ c -> m $ \ x -> runPar (k x) c
  -- fail s = Par $ \_ -> fail s

instance MonadTrans Par where
  lift m = Par $ \k s -> m >>= \a -> k a s

instance MonadIO m => MonadIO (Par m) where
  liftIO = lift . liftIO

-- dangerous
instance PrimMonad m => PrimMonad (Par m) where
  type PrimState (Par m) = PrimState m
  primitive f = lift (primitive f)

instance MonadState s m => MonadState s (Par m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader e m => MonadReader e (Par m) where
  ask = lift ask
  local f (Par m) = Par $ \k s -> do
    r <- ask
    local f $ m (\a -> local (const r) . k a) s

instance
  ( MonadRef m
  , MonadLogic m
  ) => MonadLogic (Par m) where
  msplit m = fmap parState <$> parState (msplit (statePar m))

apply :: (a -> b, a) -> b
apply (f,x) = f x

-- halt and catch fire
hcf :: Applicative m => Task m -- ParEnv m -> m (ParEnv m)
hcf s = apply $ s & todo %%~ \xss -> case uncons xss of
  x :&: xs -> (x, xs)
  Empty -> (pure, Nil)

instance MonadCont (Par m) where
  callCC f = Par $ \ c -> runPar (f (\ x -> Par $ \ _ -> c x)) c

instance Monad m => MonadPar (Par m) where
  yield = Par $ \k s ->
    apply $ s & todo %%~ \xss -> case uncons xss of
      x :&: xs -> (x, xs `snoc` k ())
      Empty    -> (k (), Nil)

  halt = Par $ \_ -> hcf
  fork m = parState $ todo %= \ys -> snoc ys $ runPar m $ \_ -> hcf

instance MonadFail m => MonadFail (Par m) where
  fail s = Par $ \_ _ -> fail s

instance Alternative m => Alternative (Par m) where
  empty = Par $ \_ _ -> empty
  Par m <|> Par n = Par $ \k s -> m k s <|> n k s

instance MonadPlus m => MonadPlus (Par m) where
  mzero = Par $ \_ _ -> mzero
  Par m `mplus` Par n = Par $ \k s -> m k s `mplus` n k s
