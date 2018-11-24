{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language BangPatterns #-}
{-# language TemplateHaskell #-}

module Log where

import Control.Lens
import Data.FingerTree as F

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
--
-- TODO: allow some form of properly filtered threshold reads in the log
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

-- clone the oldest version in the log, or the beginning of time if newLog True was used to construct this
watchOld :: Monoid a => Log a -> (Int, Log a)
watchOld (Log t v c m) = case viewl t of
  EmptyL -> (v, Log t v (c+1) m)
  LogEntry ov oc a F.:< t' -> (ov, Log (LogEntry ov (oc+1) a F.<| t') v c m)

