{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
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

module Cell
  ( Propagator, newPropagator, fire
  , Cell, newCell, writeCell, ground
  -- utility
  , Cells, Propagators
  , propSources, propTargets
  ) where

import Control.Monad.State
import Control.Lens
import Data.IntSet as IntSet
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Maybe
import Data.Set as Set -- HashSet?
import Key
import Log
import Ref
import Skew

type Cells = IntSet
type Propagators m = Set (Propagator m)  -- TODO: newtype this, users see it

data Propagator m = Propagator
  { _propagatorAction :: m (Propagators m)
  , _propSources, _propTargets :: Cells -- TODO: added for future topological analysis 
  , _propagatorId     :: {-# unpack #-} !Int
  }

propSources, propTargets :: Propagator m -> Cells
propSources = _propSources
propTargets = _propTargets

-- pattern Prop :: m (Propagators m) -> Cells -> Cells -> Int -> Propagator m
-- pattern Prop m s t i <- Propagator m s t i

instance Eq (Propagator m) where
  (==) = (==) `on` _propagatorId

instance Ord (Propagator m) where
  compare = compare `on` _propagatorId

data Val m = Val
  { _cellPropagators :: Propagators m -- outbound propagators
  , _cellStrategy    :: m () -- this forces us to be present and grounded
  }

-- makeLenses ''Propagator
makeLenses ''Val

data Cell m a c = Cell 
  { _varId :: {-# unpack #-} !Int
  , _varUpdate :: (a -> m (Maybe c))
  , _varLog :: {-# unpack #-} !(Log (KeyState m) c)
  }

instance Eq (Cell m a c) where
  (==) = (==) `on` _varId

instance Ord (Cell m a c) where
  compare = compare `on` _varId

data Env m = Env !(Skew (Val m)) !Int !(RefEnv (KeyState m))

class HasRefEnv s (KeyState m) => HasEnv s m | s -> m where
  env :: Lens' s (Env m)

  cells :: Lens' s (Skew (Val m))
  cells = env.cells

  freshPropagatorId :: Lens' s Int
  freshPropagatorId = env.freshPropagatorId

instance (u ~ KeyState m) => HasRefEnv (Env m) u where
  refEnv f (Env c p r) = Env c p <$> f r

instance HasEnv (Env m) m where
  env = id
  cells f (Env c p r) = f c <&> \c' -> Env c' p r
  freshPropagatorId f (Env c p r) = f p <&> \p' -> Env c p' r

cell :: (HasEnv s m, Applicative m) => Cell m a c -> Lens' s (Val m)
cell (Cell j _ _) f = cells (var j f') where
  f' = fmap Just . f . fromMaybe (Val mempty (pure ()))

newCell
  :: (MonadState s m, HasEnv s m, MonadKey m, Monoid c)
  => (a -> m (Maybe c)) -- ^ filtered update function
  -> Bool               -- ^ keep all history
  -> Maybe (m ())       -- ^ grounding strategy
  -> m (Cell m a c)
newCell u k mstrat = do
  j <- cells %%= allocate 1
  for_ mstrat $ \strat -> cells.var j ?= Val mempty strat
  Cell j u <$> newLog k

writeCell :: (MonadState s m, HasEnv s m) => Cell m a c -> a -> m ()
writeCell v@(Cell _ u l@Log{}) a = u a >>= \case
  Nothing -> pure ()
  Just c -> do
    record l c -- add this change to the log
    use (cell v . cellPropagators) >>= fire

-- horrible but valid schedule, til we have non-monotonic edges
fire :: (MonadState s m, HasEnv s m) => Propagators m -> m ()
fire ps = forM_ (Set.maxView ps) $ \ (Propagator m _ _ _, ps') -> do
  qs <- m
  fire (ps' <> qs) 

newPropagator
  :: (MonadState s m, HasEnv s m, Foldable f)
  => Cells -- ^ sources
  -> Cells -- ^ targets
  -> m (Propagators m) -- ^ propagator action
  -> m (Propagator m)
newPropagator cs ds act = do
  p <- Propagator act cs ds <$> (freshPropagatorId <<+= 1)
  for_ (IntSet.toList cs) $ \c ->
    cells.var c.anon (Val mempty (pure ())) (const False) . cellPropagators %= Set.insert p
  pure p

ground :: (MonadState s m, HasEnv s m) => m ()
ground = use cells >>= sequenceOf_ (traverse.cellStrategy)
