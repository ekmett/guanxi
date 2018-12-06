{-# language AllowAmbiguousTypes #-} {-# language DefaultSignatures #-}
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


-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Ref.Signal
  ( Signal(..)
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
  ) where

import Control.Monad.State
import Control.Lens
import Data.IntSet as IntSet
import Data.Default
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Kind
import Data.Proxy
import Data.Set as Set -- HashSet?
import Ref.Env as Env

newtype Signals (m :: Type -> Type) = Signals { getSignals :: IntSet }
  deriving (Semigroup, Monoid)

type role Signals nominal

type Propagators m = Set (Propagator m)

data Propagator m = Propagator
  { _propagatorAction :: m () -- TODO: return if we should self-delete, e.g. if all inputs are covered by contradiction
  , _propSources, _propTargets :: !(Signals m) -- TODO: added for future topological analysis
  , propagatorId :: {-# unpack #-} !Int
  }

instance Eq (Propagator m) where
  (==) = (==) `on` propagatorId

instance Ord (Propagator m) where
  compare = compare `on` propagatorId

data Cell m = Cell
  { _cellPropagators :: Propagators m -- outbound propagators
  , _cellStrategy    :: m () -- this forces us to be present and grounded
  }

type role Cell nominal

instance Applicative m => Semigroup (Cell m) where
  Cell p s <> Cell q t = Cell (p <> q) (s *> t)

makeLenses ''Cell

data SignalEnv m = SignalEnv
  !(Env (Cell m))
  !Int
  !(Propagators m) -- pending propagators
  !Bool

instance Default (SignalEnv m) where
  def = SignalEnv def 0 mempty False

class HasSignalEnv s m | s -> m where
  signalEnv :: Lens' s (SignalEnv m)

  -- these could become references, threaded onto a singly linked list in the env
  cells :: Lens' s (Env (Cell m))
  cells = signalEnv.cells

  freshPropagatorId :: Lens' s Int
  freshPropagatorId = signalEnv.freshPropagatorId

  pending :: Lens' s (Propagators m)
  pending = signalEnv.pending

  -- if we move to a reader discipline then this being updated only with local makes sense
  safety :: Lens' s Bool
  safety = signalEnv.safety

instance HasSignalEnv (SignalEnv m) m where
  signalEnv = id
  cells f (SignalEnv c p pp s) = f c <&> \c' -> SignalEnv c' p pp s
  freshPropagatorId f (SignalEnv c p pp s) = f p <&> \p' -> SignalEnv c p' pp s
  -- TODO: writing to the pending list with the safety off is dangerous, fix this?
  pending f (SignalEnv c p pp s) = f pp <&> \pp' -> SignalEnv c p pp' s
  safety f (SignalEnv c p pp s) = f s <&> SignalEnv c p pp

class HasSignals m t | t -> m where
  signals :: t -> Signals m

instance (m ~ n) => HasSignals m (Proxy n) where
  signals = mempty

instance (m ~ n) => HasSignals m (Signals n) where
  signals = id

newtype Signal (m :: * -> *) = Signal { getSignal :: Int }
  deriving (Eq, Ord, Show)

instance HasSignals m (Signal m) where
  signals (Signal i) = Signals (IntSet.singleton i)

newSignal_ :: (MonadState s m, HasSignalEnv s m) => m (Signal m)
newSignal_ = Signal <$> (cells %%= allocate 1)

newSignal :: (MonadState s m, HasSignalEnv s m) => (Signal m -> m ()) -> m (Signal m)
newSignal strat = do
  j@(Signal -> vj) <- cells %%= allocate 1
  cells.at j ?= Cell mempty (strat vj)
  pure vj

scope :: (MonadState s m, HasSignalEnv s m) => m a -> m a
scope m = join $ safety %%= \s -> (,True) $ do
  a <- m -- run m with the safety turned on, so we delay firings
  unless s $ do -- if the safety was already on do nothing
    safety .= False -- otherwise turn it back off and fire as needed
    fire_
  pure a

-- fire _now_
fire__ :: (MonadState s m, HasSignalEnv s m) => m ()
fire__ = join $ pending %%= \ ps -> case Set.maxView ps of
  Just (Propagator m _ _ _, ps') -> (m *> fire__, ps')
  Nothing -> (pure (), ps)

-- horrible but valid schedule, til we have non-monotonic edges at least
fire_ :: (MonadState s m, HasSignalEnv s m) => m ()
fire_ = do
  s <- use safety
  unless s fire__

fire :: (MonadState s m, HasSignalEnv s m, HasSignals m v) => v -> m ()
fire v = scope $
  for_ (IntSet.toList $ getSignals $ signals v) $ \i -> use (cells.at i) >>= \case
    Nothing -> pure ()
    Just (Cell ps _) -> pending <>= ps

propagate
  :: (MonadState s m, HasSignalEnv s m, HasSignals m x, HasSignals m y)
  => x -- ^ sources
  -> y -- ^ targets
  -> m () -- ^ propagator action
  -> m ()
propagate (signals -> cs) (signals -> ds) act = do
  p <- Propagator act cs ds <$> (freshPropagatorId <<+= 1)
  for_ (IntSet.toList $ getSignals cs) $ \c ->
    cells.at c.anon (Cell mempty (pure ())) (const False) . cellPropagators %= Set.insert p

ground :: (MonadState s m, HasSignalEnv s m) => m ()
ground = use cells >>= sequenceOf_ (traverse.cellStrategy)
