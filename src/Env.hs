{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
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

-- * Utilities
watchNew :: Monoid a => FingerTree (LogEntry a) (LogEntry a) -> Int -> Maybe a -> State Int (Log a)
watchNew t c m = state $ \v -> case m of
  Nothing -> (Log t v (c+1) Nothing, v)
  Just a  -> (Log (t F.|> LogEntry v c a) v' 1 Nothing, v') where !v' = v + 1

-- clone the oldest version in the log
watchOld :: Monoid a => FingerTree (LogEntry a) (LogEntry a) -> Int -> Maybe a -> State Int (Log a)
watchOld t c m = state $ \v -> case viewl t of
  EmptyL                 -> (Log t v (c+1) m, v)
  LogEntry ov oc a F.:< t' -> (Log (LogEntry ov (oc+1) a F.<| t') v c m, ov)


-- a commutative monoid a with an inflationary action on an ordered set b
-- with the filtered deltas described by c
class (Monoid a, Monoid (Filtered a)) => ConjoinedSemilattice a where
  type Lattice a :: *
  type Filtered a :: *

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
  Propagator :: (PropagatorId -> m (Set PropagatorId)) -> Set CellId -> Propagator m

data Cell where
  Cell :: (Typeable a, ConjoinedSemilattice a) =>
     { cellProxy :: !(Proxy a)
     , cellValue :: Lattice a
     , cellLog   :: Log (Filtered a)
     , cellPropagators :: Set PropagatorId
     } -> Cell

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
    Just (Propagator k _) -> do
      ps'' <- k p
      fire (ps' <> ps'')

unsafePeek :: forall a s m. (MonadState s m, HasEnv s m) => Var a -> m (Lattice a)
unsafePeek (Var (CellId i)) = use (cells.var i) <&> \case
  Just (Cell (_ :: Proxy b) v _ _) -> case eqT @a @b of
    Just Refl -> v
    Nothing -> error "illegal heap state, you have a var that doesn't apply"
  Nothing -> bottom @a

{-
-- * Cursors

data Cursor a = Cursor {-# UNPACK #-} !(Log a) {-# UNPACK #-} !(IORef Int)

-- | Subscribe to _new_ updates, but we won't get the history.
newCursor :: Monoid a => Log a -> IO (Cursor a)
newCursor l@(Log lp) = mask_ $ do
  n <- atomicModifyIORef' lp $ \(LogState t v c m) -> watchNew t v c m
  p <- newIORef n
  let result = Cursor l p
  _ <- mkWeakIORef p $ deleteCursor result
  return result

-- | Subscribe to the oldest updates available. If we allocated our 'newLog' as persistent this will hold everything.
oldCursor :: Monoid a => Log a -> IO (Cursor a)
oldCursor l@(Log lp) = mask_ $ do
  n <- atomicModifyIORef' lp $ \(LogState t v c m) -> watchOld t v c m
  p <- newIORef n
  let result = Cursor l p
  _ <- mkWeakIORef p $ deleteCursor result
  return result

data InvalidCursor = InvalidCursor deriving (Show,Exception)


-- 0 m 1 m {2 mempty} 3 m 4 m {5 mempty} {6 mempty} {7 mempty} 8 m {9 mempty} 10 Maybe
-- {}'d things are implied
advance :: Monoid a => Cursor a -> IO a
advance (Cursor (Log lp) p) = readIORef p >>= \i -> if
  | i < 0 -> throwIO InvalidCursor -- this cursor has been deleted
  | otherwise -> mask_ $ do
    (j,r) <- atomicModifyIORef lp $ \ls@(LogState t v c m) -> if
      | i >= v -> case m of
        Nothing -> (ls,(i,mempty)) -- nothing new, stay put
        Just a
          | c == 1 -> case viewr t of
            t' :> LogEntry ov oc b -> (LogState (t' |> LogEntry ov oc (mappend b a)) v 1 Nothing,(v,a)) -- merge
            EmptyR                 -> (LogState t v c Nothing,(i,a)) -- we're the only one listening
          | !v' <- v + 1           -> (LogState (t |> LogEntry v (c-1) a) v' 1 Nothing,(v',a)) -- observe and bump
      | otherwise -> case split (\(LogEntry j _ _) -> j >= i) t of
        (l,r) -> case viewr l of
          EmptyR -> case watchNew t v c m of -- only fixes an "illegal" cursor? remove this?
            (ls', j) -> (ls', (j, contents (measure t) <> fold m))
          l' :> LogEntry j c' a
            | (ls', k) <- watchNew (nl >< r) v c m -> (ls', (k, a <> contents (measure r) <> fold m))
            where nl | c' > 1 = l' |> LogEntry j (c'-1) a
                     | otherwise = case viewr l' of
                       l'' :> LogEntry k c'' b -> l'' |> LogEntry k c'' (mappend b a)
                       EmptyR -> mempty -- forget history before the first cursor
    -- 'mask_' required because if we catch an async exception between the modifyIORef above and this write
    -- we'd invalidate this cursor
    writeIORef p j
    return r

deleteCursor :: Monoid a => Cursor a -> IO ()
deleteCursor (Cursor (Log lp) p) = do
  i <- readIORef p
  mask_ $ do
    atomicModifyIORef lp $ \ls@(LogState t v c m) -> if
      | i >= v -> (LogState t v (c-1) m, ())
      | otherwise -> case split (\(LogEntry j _ _) -> j >= i) t of
        (l,r) -> case viewr l of
          EmptyR -> (ls,())
          l' :> LogEntry j c' a -> (LogState (nl >< r) v c m, ()) where
            nl | c' > 1 = l' |> LogEntry j (c'-1) a
               | otherwise = case viewr l' of
                 EmptyR -> mempty
                 l'' :> LogEntry k c'' b -> l'' |> LogEntry k c'' (mappend b a)
    writeIORef p (-1) -- write an illegal version number.

-- | We haven't called deleteCursor on this thing yet, have we?
validCursor :: Cursor a -> IO Bool
validCursor (Cursor _ p) = (>= 0) <$> readIORef p

-}

