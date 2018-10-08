{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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

module Env where

import Control.Arrow (first)
import Control.Monad.State
import Control.Lens
import Data.Set -- HashSet?
import Data.FingerTree as F
import Data.Foldable as Foldable
import Data.Kind
import Data.Maybe
import Data.Typeable
import Skew

-- version # since, ref count, monoidal summary
data LogEntry a = LogEntry
  { since, refCount :: {-# unpack #-} !Int
  , contents :: a
  } deriving Show

instance Monoid a => Measured (LogEntry a) (LogEntry a) where
  measure = id

instance Semigroup a => Semigroup (LogEntry a) where
  LogEntry i c a <> LogEntry j d b = LogEntry (max i j) (c + d) (a <> b)

instance Monoid a => Monoid (LogEntry a) where
  mempty = LogEntry 0 0 mempty

-- the historical log, the current version number, a reference count and a value
-- the 'since' for the entry we're building here would be (current - ref count)
-- the goal is to use this to track 'fingers' into the changeset for a propagation
-- cell
data Log a = Log
  { logTree               :: !(FingerTree (LogEntry a) (LogEntry a))
  , logSince, logRefCount :: {-# UNPACK #-} !Int
  , logChanges            :: !(Maybe a)
  } deriving Show

makeClassy ''Log

-- | Argument tracks if we're persistent or not. If persistent then 'oldCursors' can start at the beginning
-- otherwise we won't collect history beyond what is needed to support active cursors.
newLog :: Monoid a => Bool -> Log a
newLog p = Log mempty 0 (fromEnum p) Nothing

record :: Semigroup a => a -> Log a -> Log a
record a ls@(Log t v c m)
  | F.null t, c == 0 = ls -- nobody is watching
  | otherwise = Log t v c $ Just $ maybe a (<> a) m

-- | Get a snapshot of the # of cursors outstanding
cursors :: Monoid a => Log a -> Int
cursors (Log t _ c _) = refCount (measure t) + c

watchNew :: Monoid a => Log a -> (Int, Log a)
watchNew (Log t v c m) = case m of
  Nothing -> (v, Log t v (c+1) Nothing)
  Just a  -> (v', Log (t F.|> LogEntry v c a) v' 1 Nothing) where !v' = v + 1

-- clone the oldest version in the log
watchOld :: Monoid a => Log a -> (Int, Log a)
watchOld (Log t v c m) = case viewl t of
  EmptyL -> (v, Log t v (c+1) m)
  LogEntry ov oc a F.:< t' -> (ov, Log (LogEntry ov (oc+1) a F.<| t') v c m)

-- a commutative monoid a with an inflationary action on an ordered set b
-- with the filtered deltas described by c
class (Monoid a, Monoid (Filtered a)) => ConjoinedSemilattice a where
  type Lattice a :: Type
  type Filtered a :: Type

  bottom :: Lattice a
  default bottom :: (a ~ Lattice a) => Lattice a
  bottom = mempty

  act :: a -> Lattice a -> Maybe (Lattice a, Filtered a) -- tells me if it updated and what to pass to propagators
  default act :: (a ~ Lattice a, a ~ Filtered a, Ord a) => a -> Lattice a -> Maybe (Lattice a, Filtered a)
  act a b = (c, c) <$ guard (c > b) where c = mappend a b

type Id = Int

newtype PropagatorId = PropagatorId Id
  deriving (Eq,Ord,Show,Read)

newtype CellId = CellId Id
  deriving (Eq,Ord,Show,Read)

data Propagator m where
  Propagator :: Typeable s =>
    { propagatorAction :: PropagatorId -> s -> m (Set PropagatorId, s)
    , propagatorState :: s
    , propagatorSources :: Set CellId
    } -> Propagator m

data Cell where
  Cell :: (Typeable a, ConjoinedSemilattice a) =>
     { cellProxy :: {-# UNPACK #-} !(Proxy a)
     , cellValue :: Lattice a
     , cellLog   :: Log (Filtered a)
     , cellPropagators :: Set PropagatorId
     } -> Cell

data KnownCell a = KnownCell
  { _knownCellValue :: Lattice a
  , _knownCellLog :: Log (Filtered a)
  , _knownCellPropagators :: Set PropagatorId
  }

makeClassy ''KnownCell

-- this backtracks across threads
data Env m = Env
  { _cells :: !(Skew Cell) -- this requires cells to know their update strategy, i can't stub
  , _propagators :: !(Skew (Propagator m))
  }

makeClassy ''Env

data Var a where
  Var :: (Typeable a, ConjoinedSemilattice a) => CellId -> Var a

-- we'll store things in state for now. TODO: use some form of update monad
newVar :: (MonadState s m, HasEnv s m, Typeable a, ConjoinedSemilattice a) => m (Var a)
newVar = state $ cells $ first (Var . CellId) . allocate 1

writeVar :: forall a s m. (MonadState s m, HasEnv s m) => Var a -> a -> m ()
writeVar (Var (CellId i)) a = do
  Cell (_ :: Proxy b) cv cl cp
    <- uses (cells.var i) $ fromMaybe $ Cell (Proxy :: Proxy a) (bottom @a) (newLog False) mempty
  case eqT @a @b of
    Nothing -> error "illegal heap state, you have a var that doesn't apply"
    Just Refl -> forM_ (act a cv) $ \(cv', c) -> do
        cells.var i ?= Cell (Proxy :: Proxy a) cv' (record c cl) cp
        fire cp

-- horrible but valid schedule
fire :: (MonadState s m, HasEnv s m) => Set PropagatorId -> m ()
fire ps = forM_ (maxView ps) $ \ (p@(PropagatorId i), ps') ->
  use (propagators.var i) >>= \case
    Nothing -> error "fired missing propagator"
    Just (Propagator k (s :: a) _) -> do
      (ps'', s') <- k p s
      propagators.var i %= \case
        Just (Propagator k' (_ :: b) cs) -> case eqT @a @b of
          Just Refl -> Just (Propagator k' s' cs)
          Nothing -> error "somebody swapped out my propagator when I wasn't looking"
        Nothing -> Nothing -- deleted in the meantime
      fire (ps' <> ps'')

-- TODO: should we detect that we wrote back a trivial cell change?
cell :: forall a s m. HasEnv s m => Var a -> Lens' s (KnownCell a)
cell (Var (CellId i)) = cells . var i . known where

  known :: Lens' (Maybe Cell) (KnownCell a)
  known f Nothing = forget <$> f (KnownCell (bottom @a) (newLog False) mempty)
  known f (Just (Cell (_ :: Proxy b) cb cl cp)) = case eqT @a @b of
      Just Refl -> forget <$> f (KnownCell cb cl cp)
      Nothing -> error "Env.cell: bad Var!"

  forget :: KnownCell a -> Maybe Cell
  forget (KnownCell cb cl cp) = Just $ Cell (Proxy @a) cb cl cp

peek :: (MonadState s m, HasEnv s m) => Var a -> m (Lattice a)
peek v = use (cell v . knownCellValue)

data Cursor a = Cursor {-# UNPACK #-} !(Var a) {-# UNPACK #-} !Int

-- | Subscribe to _new_ updates, but we won't get the history.
newCursor :: (MonadState s m, HasEnv s m) => Var a -> m (Cursor a)
newCursor v@Var{} = Cursor v <$> state (cell v $ knownCellLog watchNew)

oldCursor :: (MonadState s m, HasEnv s m) => Var a -> m (Cursor a)
oldCursor v@Var{} = Cursor v <$> state (cell v $ knownCellLog watchOld)

deleteCursor :: (MonadState s m, HasEnv s m) => Cursor a -> m ()
deleteCursor (Cursor x@Var{} i) = cell x . knownCellLog %= \ls@(Log t v c m) -> if
  | i >= v -> Log t v (c-1) m
  | otherwise -> case F.split (\(LogEntry j _ _) -> j >= i) t of
    (l,r) -> case viewr l of
      EmptyR -> ls
      l' F.:> LogEntry j c' a -> Log (nl >< r) v c m where
        nl | c' > 1 = l' F.|> LogEntry j (c'-1) a
           | otherwise = case viewr l' of
             EmptyR -> mempty
             l'' F.:> LogEntry k c'' b -> l'' F.|> LogEntry k c'' (mappend b a)

-- (0{2} m) (2{3} m) (3{4}) m {1} Nothing

-- 0 m 1 m {2 mempty} 3 m 4 m {5 mempty} {6 mempty} {7 mempty} 8 m {9 mempty} 10 Maybe
-- {}'d things are implied
advance :: (MonadState s m, HasEnv s m) => Cursor a -> m (Filtered a, Cursor a)
advance old@(Cursor x@Var{} i) = cell x . knownCellLog %%= \ls@(Log t v c m) -> if
  | i >= v -> case m of
    Nothing -> ((mempty, old), ls)
    Just a
      | c == 1 -> case viewr t of
        t' F.:> LogEntry ov oc b -> ((a,old),Log (t' F.|> LogEntry ov oc (mappend b a)) v 1 Nothing)
        EmptyR -> ((a,old), Log t v c Nothing) -- we're the only one listening
      | !v' <- v + 1 -> ((a, Cursor x v'), Log (t F.|> LogEntry v (c-1) a) v' 1 Nothing)
  | otherwise -> case F.split (\(LogEntry j _ _) -> j >= i) t of
    (l,r) -> case viewr l of
      EmptyR -> error "missing cursor!"
      l' F.:> LogEntry j c' a
        | (ls', k) <- watchNew (Log (nl >< r) v c m) -> ((a <> contents (measure r) <> Foldable.fold m, Cursor x ls'), k) where
        nl | c' > 1 = l' F.|> LogEntry j (c'-1) a
           | otherwise = case viewr l' of
             l'' F.:> LogEntry k c'' b -> l'' F.|> LogEntry k c'' (mappend b a)
             EmptyR -> mempty
