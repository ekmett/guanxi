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

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Cell
  ( Propagator(..), newPropagator, newPropagator_
  , aim, fire_
  , Var(..), newVar, newVar_, fire
  , ground
  , Sink(..), writeSink
  , HasCellIds(..)
  , HasCellEnv(..), CellEnv, defaultCellEnv
  -- utility
  , Cells, Propagators
  ) where

import Control.Monad.State
import Control.Lens
import Data.IntSet as IntSet
import Data.Foldable as Foldable
import Data.Functor.Contravariant.Divisible
import Data.Function (on)
import Data.Set as Set -- HashSet?
import Data.Void
import Internal.Skew as Skew
import Key
import Ref

type Cells = IntSet -- TODO: newtype this, users see it
type Propagators m = Set (Propagator m)  -- TODO: newtype this, users see it

data Propagator m = Propagator
  { propagatorAction :: m ()
  , propSources, propTargets :: !Cells -- TODO: added for future topological analysis 
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

makeLenses ''Cell

data Sink m a = Sink 
  { _cellIds    :: !Cells
  , _cellUpdate :: a -> m () -- update
  }

instance Contravariant (Sink m) where
  contramap f (Sink m g) = Sink m (g . f)
 
instance Applicative m => Divisible (Sink m) where
  conquer = Sink mempty $ \_ -> pure ()
  divide f (Sink s g) (Sink t h) = Sink (s <> t) $ \a -> case f a of 
     (b, c) -> g b *> h c

instance Applicative m => Decidable (Sink m) where
  lose f = Sink mempty (absurd . f)
  choose f (Sink s g) (Sink t h) = Sink (s <> t) $ \a -> case f a of
     Left b -> g b
     Right c -> h c

data CellEnv m = CellEnv
  !(Skew (Cell m))
  !Int
  !(Propagators m) -- pending propagators
  !(RefEnv (KeyState m))
  !Bool
  -- TODO: track a deferral flag to delay firing, when we run actions with a fire_ to come after
  -- we'll set the flag, letting us batch more activity. this can be more useful once we have a topological
  -- sort and propagators that exploit logs
 
defaultCellEnv :: CellEnv m
defaultCellEnv = CellEnv Skew.empty 0 mempty defaultRefEnv False

class HasRefEnv s (KeyState m) => HasCellEnv s m | s -> m where
  cellEnv :: Lens' s (CellEnv m)

  cells :: Lens' s (Skew (Cell m))
  cells = cellEnv.cells

  freshPropagatorId :: Lens' s Int
  freshPropagatorId = cellEnv.freshPropagatorId
  
  pending :: Lens' s (Propagators m)
  pending = cellEnv.pending

  safety :: Lens' s Bool
  safety = cellEnv.safety

instance (u ~ KeyState m) => HasRefEnv (CellEnv m) u where
  refEnv f (CellEnv c p pp r s) = f r <&> \r' -> CellEnv c p pp r' s

instance HasCellEnv (CellEnv m) m where
  cellEnv = id
  cells f (CellEnv c p pp r s) = f c <&> \c' -> CellEnv c' p pp r s
  freshPropagatorId f (CellEnv c p pp r s) = f p <&> \p' -> CellEnv c p' pp r s
  -- TODO: writing to the pending list with the safety off is dangerous, fix this?
  pending f (CellEnv c p pp r s) = f pp <&> \pp' -> CellEnv c p pp' r s
  safety f (CellEnv c p pp r s) = f s <&> CellEnv c p pp r

class HasCellIds t where
  cellIds :: t -> Cells

instance HasCellIds () where
  cellIds = mempty

instance HasCellIds (Sink m a) where
  cellIds (Sink s _) = s

instance HasCellIds Cells where
  cellIds = id

newtype Var (m :: * -> *) = Var { getVar :: Int }
  deriving (Eq, Ord, Show)

instance HasCellIds (Var m) where
  cellIds (Var i) = IntSet.singleton i

newVar_ :: (MonadState s m, HasCellEnv s m) => m (Var m)
newVar_ = Var <$> (cells %%= allocate 1)

newVar :: (MonadState s m, HasCellEnv s m) => (Var m -> m ()) -> m (Var m)
newVar strat = do 
  j@(Var -> vj) <- cells %%= allocate 1
  cells.var j ?= Cell mempty (strat vj)
  pure vj

writeSink :: (MonadState s m, HasCellEnv s m) => Sink m a -> a -> m ()
writeSink (Sink _ u) a = scope (u a)

aim :: (MonadState s m, HasCellEnv s m) => Propagator m -> m ()
aim p = pending %= Set.insert p

scope :: (MonadState s m, HasCellEnv s m) => m a -> m a
scope m = join $ safety %%= \s -> (,True) $ do
  a <- m -- run m with the safety turned on, so we delay firings
  unless s $ do -- if the safety was already on do nothing
    safety .= False -- otherwise turn it back off and fire as needed
    fire_
  pure a

-- fire _now_
fire__ :: (MonadState s m, HasCellEnv s m) => m ()
fire__ = join $ pending %%= \ ps -> case Set.maxView ps of
  Just (Propagator m _ _ _, ps') -> (m *> fire__, ps')
  Nothing -> (pure (), ps)

-- horrible but valid schedule, til we have non-monotonic edges at least
fire_ :: (MonadState s m, HasCellEnv s m) => m ()
fire_ = do
  s <- use safety
  unless s fire__ 

fire :: (MonadState s m, HasCellEnv s m, HasCellIds v) => v -> m ()
fire v = scope $ 
  for_ (IntSet.toList (cellIds v)) $ \i -> use (cells.var i) >>= \case
    Nothing -> pure ()
    Just (Cell ps _) -> pending <>= ps

newPropagator
  :: (MonadState s m, HasCellEnv s m, HasCellIds x, HasCellIds y)
  => x -- ^ sources
  -> y -- ^ targets
  -> m () -- ^ propagator action
  -> m (Propagator m)
newPropagator (cellIds -> cs) (cellIds -> ds) act = do
  p <- Propagator act cs ds <$> (freshPropagatorId <<+= 1)
  for_ (IntSet.toList cs) $ \c ->
    cells.var c.anon (Cell mempty (pure ())) (const False) . cellPropagators %= Set.insert p
  pure p

newPropagator_ 
  :: (MonadState s m, HasCellEnv s m, HasCellIds x, HasCellIds y)
  => x -- ^ sources
  -> y -- ^ targets
  -> m () -- ^ propagator action
  -> m ()
newPropagator_ x y m = void (newPropagator x y m)

ground :: (MonadState s m, HasCellEnv s m) => m ()
ground = use cells >>= sequenceOf_ (traverse.cellStrategy)
