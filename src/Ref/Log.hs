{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language BangPatterns #-}
{-# language TemplateHaskell #-}
{-# language MultiWayIf #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language DeriveTraversable #-}
{-# language ScopedTypeVariables #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Ref.Log 
  ( 
  -- * Logs
    Log(..)
  , newLog, cursors, record
  -- * Cursors
  , Cursor, newCursor, oldCursor, deleteCursor, advance
  ) where

import Control.Monad (join)
import Control.Monad.State.Class
import Control.Lens
import Data.FingerTree as F
import Data.Foldable as Foldable
import Prelude hiding (log)
import Ref.Base
import Ref.Key

-- version # since, ref count, monoidal summary
data LogEntry a = LogEntry
  { since, refCount :: {-# unpack #-} !Int
  , contents :: a
  } deriving (Show, Functor, Foldable, Traversable)

instance Semigroup a => Measured (LogEntry (Maybe a)) (LogEntry a) where
  measure = fmap Just 

instance Semigroup a => Semigroup (LogEntry a) where
  LogEntry i c a <> LogEntry j d b = LogEntry (max i j) (c + d) (a <> b)

instance Monoid a => Monoid (LogEntry a) where
  mempty = LogEntry 0 0 mempty

-- the historical log, the current version number, a reference count and a value
-- the 'since' for the entry we're building here would be (current - ref count)
-- the goal is to use this to track 'fingers' into the changeset for a propagation
-- cell
--
-- TODO: allow some form of properly filtered threshold reads in the log
data LogState a = LogState
  { logTree               :: !(FingerTree (LogEntry (Maybe a)) (LogEntry a))
  , logSince, logRefCount :: {-# UNPACK #-} !Int
  , logChanges            :: !(Maybe a)
  } deriving Show

-- | Argument tracks if we're persistent or not. If persistent then 'oldCursors' can start at the beginning
-- otherwise we won't collect history beyond what is needed to support active cursors.
newLogState :: Semigroup a => Bool -> LogState a
newLogState p = LogState mempty 0 (fromEnum p) Nothing

recordState :: Semigroup a => a -> LogState a -> LogState a
recordState a ls@(LogState t v c m)
  | F.null t, c == 0 = ls -- nobody is watching
  | otherwise = LogState t v c $ Just $ maybe a (<> a) m

--cursors :: Monoid a => LogState a -> Int
--cursors (LogState t _ c _) = refCount (measure t) + c

watchNew :: Semigroup a => LogState a -> (Int, LogState a)
watchNew (LogState t v c m) = case m of
  Nothing -> (v, LogState t v (c+1) Nothing)
  Just a  -> (v', LogState (t F.|> LogEntry v c a) v' 1 Nothing) where !v' = v + 1

-- clone the oldest version in the log, or the beginning of time if newLogState True was used to construct this
watchOld :: Semigroup a => LogState a -> (Int, LogState a)
watchOld (LogState t v c m) = case viewl t of
  EmptyL -> (v, LogState t v (c+1) m)
  LogEntry ov oc a F.:< t' -> (ov, LogState (LogEntry ov (oc+1) a F.<| t') v c m)
 
data Log u a where
  Log :: Semigroup a => { getLog :: Ref u (LogState a) } -> Log u a

log :: HasRefEnv s u => Log u a -> Lens' s (LogState a)
log = ref . getLog

newLog
  :: (MonadState s m, MonadKey m, HasRefEnv s (KeyState m), Semigroup a)
  => Bool
  -> m (Log (KeyState m) a)
newLog = fmap Log . newRef . newLogState

-- | Get a snapshot of the # of cursors outstanding
cursors :: (MonadState s m, HasRefEnv s u) => Log u a -> m Int
cursors l@Log{} = uses (log l) $ \(LogState t _ c _) -> refCount (measure t) + c 

record :: (MonadState s m, HasRefEnv s u) => Log u a -> a -> m ()
record (Log r) a = modifyRef r (recordState a)

data Cursor u a = Cursor {-# unpack #-} !(Log u a) {-# unpack #-} !(Ref u Int)

-- data Cursor a = Cursor !(Var a) {-# UNPACK #-} !Int

-- | Subscribe to _new_ updates, but we won't get the history.
newCursor, oldCursor
  :: (MonadState s m, HasRefEnv s (KeyState m), MonadKey m)
  => Log (KeyState m) a
  -> m (Cursor (KeyState m) a)
newCursor l@Log{} = Cursor l <$> do
  i <- log l %%= watchNew 
  newRef i

oldCursor l@Log{} = Cursor l <$> do
  i <- log l %%= watchOld
  newRef i

-- invalidates this cursor
deleteCursor :: (MonadState s m, HasRefEnv s u) => Cursor u a -> m ()
deleteCursor (Cursor rl@Log{} ri) = do
  i <- readRef ri
  unsafeDeleteRef ri -- we're done, invalidate this cursor
  log rl %= \ls@(LogState t v c m) -> if
    | i >= v -> LogState t v (c-1) m
    | otherwise -> case F.split (\(LogEntry j _ _) -> j >= i) t of
      (l,r) -> case viewr l of
        EmptyR -> ls
        l' F.:> LogEntry j c' a -> LogState (nl >< r) v c m where
          nl | c' > 1 = l' F.|> LogEntry j (c'-1) a
             | otherwise = case viewr l' of
               EmptyR -> mempty
               l'' F.:> LogEntry k c'' b -> l'' F.|> LogEntry k c'' (b <> a)

-- (0{2} m) (2{3} m) (3{4}) m {1} Nothing

-- 0 m 1 m {2 mempty} 3 m 4 m {5 mempty} {6 mempty} {7 mempty} 8 m {9 mempty} 10 Maybe
-- {}'d things are implied
advance :: forall s m u a. (MonadState s m, HasRefEnv s u) => Cursor u a -> m (Maybe a)
advance (Cursor rl@Log{} ri) = do
  i <- readRef ri 
  join $ log rl %%= \ls@(LogState t v c (m :: Maybe a)) -> if
    | i >= v -> case m of
      Nothing -> (pure Nothing, ls)
      Just a
        | c == 1 -> case viewr t of
          t' F.:> LogEntry ov oc b -> (pure m, LogState (t' F.|> LogEntry ov oc (b <> a)) v 1 Nothing)
          EmptyR -> (pure m, LogState t v c Nothing) -- we're the only one listening
        | !v' <- v + 1 -> (m <$ writeRef ri v', LogState (t F.|> LogEntry v (c-1) a) v' 1 Nothing)
    | otherwise -> case F.split (\(LogEntry j _ _) -> j >= i) t of
      (l,r) -> case viewr l of
        EmptyR -> error "advance: missing cursor!"
        l' F.:> LogEntry j c' a
          | (ls', k) <- watchNew (LogState (nl >< r) v c m) -> ((Just a <> contents (measure r) <> m) <$ writeRef ri ls', k) where
          nl | c' > 1 = l' F.|> LogEntry j (c'-1) a
             | otherwise = case viewr l' of
               l'' F.:> LogEntry k c'' b -> l'' F.|> LogEntry k c'' (b <> a)
               EmptyR -> mempty
