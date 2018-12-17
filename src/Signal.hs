{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ViewPatterns #-}
{-# language UndecidableInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TupleSections #-}
{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RoleAnnotations #-}
{-# language ConstraintKinds #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Signal
  ( Signal(..)
  , MonadSignal
  , newSignal
  , newSignal_
  , fire, scope
  , Signals
  , HasSignals(..)
  , ground
  , propagate
  -- * implementation
  , HasSignalEnv(signalEnv)
  , SignalEnv
  , newSignalEnv
  ) where

import Control.Monad (unless)
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Lens
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Hashable
import Data.Kind
import Data.Proxy
import Data.HashSet as HashSet -- HashSet?
import Ref
import Unique

type Signals m = HashSet (Signal m)
type Propagators m = HashSet (Propagator m)

data Propagator m = Propagator
  { propagatorAction :: m () -- TODO: return if we should self-delete, e.g. if all inputs are covered by contradiction
  , _propSources, _propTargets :: !(Signals m) -- TODO: added for future topological analysis
  , propagatorId :: {-# unpack #-} !(UniqueM m)
  }

instance Eq (Propagator m) where
  (==) = (==) `on` propagatorId

instance Hashable (Propagator m) where
  hash = hash . propagatorId
  hashWithSalt d = hashWithSalt d . propagatorId

class HasSignals m t | t -> m where
  signals :: t -> Signals m

instance (m ~ n) => HasSignals m (Proxy n) where
  signals = mempty

instance (m ~ n) => HasSignals m (Signals n) where
  signals = id

data Signal (m :: Type -> Type) = Signal
  { signalId :: UniqueM m
  , signalReference :: RefM m (Propagators m)
  }

instance Eq (Signal m) where
  (==) = (==) `on` signalId

instance Hashable (Signal m) where
  hash = hash . signalId
  hashWithSalt d = hashWithSalt d . signalId

data SignalEnv m = SignalEnv
  { _signalEnvSafety  :: !Bool
  , _signalEnvPending :: !(RefM m (Propagators m)) -- pending propagators
  , _signalEnvGround  :: !(RefM m (m ())) -- final grounding action
  }

makeClassy ''SignalEnv

type MonadSignal e m = (MonadRef m, MonadReader e m, HasSignalEnv e m)

newSignalEnv :: (PrimMonad n, Monad m, PrimState m ~ PrimState n) => n (SignalEnv m)
newSignalEnv = SignalEnv False <$> newRef mempty <*> newRef (pure ())

instance (s ~ PrimState m) => Reference s (Propagators m) (Signal m) where
  reference = signalReference

instance HasSignals m (Signal m) where
  signals = HashSet.singleton

newSignal_ :: PrimMonad m => m (Signal m)
newSignal_ = Signal <$> newUnique <*> newRef mempty

newSignal :: MonadSignal e m => (Signal m -> m ()) -> m (Signal m)
newSignal strat = do
  s <- newSignal_
  g <- view signalEnvGround
  s <$ modifyRef' g (*> strat s)

scope :: MonadSignal e m => m a -> m a
scope m = do
    a <- local (signalEnvSafety .~ True) m
    SignalEnv s p _ <- view signalEnv
    a <$ unless s (go p)
  where
    go p = do
      hs <- updateRef p (,mempty)
      for_ hs propagatorAction
      unless (HashSet.null hs) (go p)

fire :: (MonadSignal e m, HasSignals m v) => v -> m ()
fire v = scope $ do
  p <- view signalEnvPending
  for_ (signals v) $ \i -> do
    ps <- readRef i
    unless (HashSet.null ps) $ modifyRef' p (<> ps) -- we could do this with a single write at the end of the scope

propagate
  :: (MonadSignal e m, HasSignals m x, HasSignals m y)
  => x -- ^ sources
  -> y -- ^ targets
  -> m () -- ^ propagator action
  -> m ()
propagate (signals -> cs) (signals -> ds) act = do
  p <- Propagator act cs ds <$> newUnique
  for_ (HashSet.toList cs) $ \c -> modifyRef' c (HashSet.insert p)

ground :: MonadSignal e m => m ()
ground = do
  g <- view signalEnvGround
  m <- readRef g
  m
