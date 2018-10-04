module Env where

import Data.Set -- HashSet?
import Data.FingerTree
import SkewMaybe

-- version # since, ref count, monoidal summary
data LogEntry a = LogEntry
  { since, refCount :: {-# unpack #-} !Int
  , contents :: a
  } deriving Show

instance Monoid a => Measured (LogEntry a) (LogEntry a) where
  measure = id

instance Monoid a => Monoid (LogEntry a) where
  mempty = LogEntry 0 0 mempty
  mappend (LogEntry i c a) (LogEntry j d b) = LogEntry (max i j) (c + d) (a <> b)

-- the historical log, the current version number, a reference count and a value 
-- the 'since' for the entry we're building here would be (current - ref count)
data Log a = Log
  { logTree               :: !(FingerTree (LogEntry a) (LogEntry a))
  , logSince, logRefCount :: {-# UNPACK #-} !Int
  , logChanges            :: !(Maybe a) 
  } deriving Show

makeClassy ''Log

-- | Argument tracks if we're persistent or not. If persistent then 'oldCursors' can start at the beginning
-- otherwise we won't collect history beyond what is needed to support active cursors.
newLog :: Monoid a => Bool -> Log a
newLog p = LogState mempty 0 (if p then 1 else 0) Nothing

record :: Monoid a => a -> Log a -> Log a
record a ls@(LogState t v c m)
  | F.null t, c == 0 = ls -- nobody is watching
  | otherwise = LogState t v c $ Just $ maybe a (`mappend` a) m

-- | Get a snapshot of the # of cursors outstanding
cursors :: Monoid a => Log a -> Int
cursors (LogState t _ c _) = refCount (measure t) + c
    
-- * Utilities

watchNew :: Monoid a => FingerTree (LogEntry a) (LogEntry a) -> Int -> Maybe a -> State Int (Log a)
watchNew t c m = state $ \v -> case m of
  Nothing -> (Log t v (c+1) Nothing, v)
  Just a  -> (Log (t |> LogEntry v c a) v' 1 Nothing, v') where !v' = v + 1

-- clone the oldest version in the log
watchOld :: Monoid a => FingerTree (LogEntry a) (LogEntry a) -> Int -> Maybe a -> State Int (Log a, Int)
watchOld t c m = state $ \v -> case viewl t of
  EmptyL                 -> (LogState t v (c+1) m, v)
  LogEntry ov oc a :< t' -> (LogState (LogEntry ov (oc+1) a <| t') v c m, ov)

record :: Semigroup c => Log c -> c -> Log c
record l c = l & changes <>~ Just c

type Id = Int

-- a commutative monoid a with an inflationary action on an ordered set b
-- with the filtered deltas described by c
class (Monoid a, Monoid c) => ConjoinedSemilattice a b c | a -> b c where
  bottom :: b
  default bottom :: (a ~ b) => b
  bottom = mempty

  act :: a -> b -> (Bool, b, c) -- tells me if it updated and what to pass to propagators
  default act :: (a ~ b, b ~ c) => a -> b -> (Bool, b, c)
  act a b = (c > b, c, c) where c = mappend a b

newtype PropagatorId = PropagatorId Int
newtype CellId = CellId Int

data Propagator m where
  Propagator :: (Id -> m (Set PropagatorId)) -> Set CellId -> Propagator m

data Cell where
  Cell :: (Typeable a, ConjoinedSemilattice a b c) => 
     { cellValue :: b
     , cellLog   :: Log c 
     , cellPropagators :: Set PropagatorId
     } -> Cell

-- this backtracks across threads
newtype Env m = Env
  { envCells       :: !(Skew Cell) -- this requires cells to know their update strategy, i can't stub
  , envPropagators :: !(Skew (Propagator m))
  }

makeClassy ''Env

newtype Var r a = Var CellId

-- we'll store things in state for now. TODO: use some form of update monad
newVar :: (MonadState s m, HasEnv s m) => m (Var a)
newVar = state $ cells $ \cs@(Env i _) -> (cons (Cell bottom act mempty mempty) cs, Var i)

write :: (MonadState s m, HasEnv s m) => Var a -> a -> m ()
write (Var cid) a = do
  Cell cv cl cp <- use (cells.var i)
  forM_ (act a cv) $ \(cv', c) -> do
    cells.var i .~ Cell cv' (record log c) cp
    fire cp

fire :: (MonadState s m, HasEnv s m) => Set PropagatorId -> m ()
fire = return () -- placeholder

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

