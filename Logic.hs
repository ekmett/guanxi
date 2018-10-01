{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}

module Logic where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Data.Functor.Identity
import Data.Foldable (fold)

class MonadPlus m => MonadLogic m where
  -- |
  -- @
  -- msplit mzero                == return Nothing
  -- msplit (return a `mplus` m) == return (Just (a, m))
  -- @
  msplit :: m a -> m (Maybe (a, m a)) 
  
  -- | fair disjunction
  interleave :: m a -> m a -> m a
  interleave m1 m2 = msplit m1 >>= maybe m2 (\(a, m1') -> return a `mplus` interleave m2 m1')

  -- | fair conjunction
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = do
    (a, m') <- maybe mzero return =<< msplit m
    interleave (f a) (m' >>- f)

  -- |
  -- @
  -- ifte (return a) th el           == th a
  -- ifte mzero th el                == el
  -- ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
  -- @
  ifte       :: m a -> (a -> m b) -> m b -> m b
  ifte t th el = msplit t >>= maybe el (\(a,m) -> th a `mplus` (m >>= th))

  -- | pruning
  once       :: m a -> m a
  once m = do
    b <- msplit m
    (a, _) <- maybe mzero return b
    return a

instance MonadLogic [] where
  msplit []     = return Nothing
  msplit (x:xs) = return $ Just (x, xs)

instance MonadLogic m => MonadLogic (ReaderT e m) where
  msplit rm = ReaderT $ \e -> msplit (runReaderT rm e) >>= \case
    Nothing -> return Nothing
    Just (a, m) -> return (Just (a, lift m))

instance MonadLogic m => MonadLogic (Strict.StateT s m) where
  msplit sm = Strict.StateT $ \s -> msplit (Strict.runStateT sm s) >>= \case
    Nothing          -> return (Nothing, s)
    Just ((a,s'), m) -> return (Just (a, Strict.StateT (\_ -> m)), s')

  interleave ma mb = Strict.StateT $ \s ->
    Strict.runStateT ma s `interleave` Strict.runStateT mb s

  ma >>- f = Strict.StateT $ \s ->
    Strict.runStateT ma s >>- \(a,s') -> Strict.runStateT (f a) s'

  ifte t th el = Strict.StateT $ \s -> ifte
    (Strict.runStateT t s)
    (\(a,s') -> Strict.runStateT (th a) s')
    (Strict.runStateT el s)

  once ma = Strict.StateT $ \s -> once (Strict.runStateT ma s)

instance MonadLogic m => MonadLogic (Lazy.StateT s m) where
  msplit sm = Lazy.StateT $ \s -> msplit (Lazy.runStateT sm s) >>= \case
    Nothing -> return (Nothing, s)
    Just ((a,s'), m) -> return (Just (a, Lazy.StateT (\_ -> m)), s')

  interleave ma mb = Lazy.StateT $ \s -> Lazy.runStateT ma s `interleave` Lazy.runStateT mb s

  ma >>- f = Lazy.StateT $ \s -> Lazy.runStateT ma s >>- \(a,s') -> Lazy.runStateT (f a) s'

  ifte t th el = Lazy.StateT $ \s -> ifte
    (Lazy.runStateT t s)
    (\(a,s') -> Lazy.runStateT (th a) s')
    (Lazy.runStateT el s)

  once ma = Lazy.StateT $ \s -> once (Lazy.runStateT ma s)

instance (MonadLogic m, Monoid w) => MonadLogic (Strict.WriterT w m) where
  msplit wm = Strict.WriterT $ msplit (Strict.runWriterT wm) >>= \case
    Nothing -> return (Nothing, mempty)
    Just ((a,w), m) -> return (Just (a, Strict.WriterT m), w)

  interleave ma mb = Strict.WriterT $
    Strict.runWriterT ma `interleave` Strict.runWriterT mb

  ma >>- f = Strict.WriterT $
    Strict.runWriterT ma >>- \(a,w) -> Strict.runWriterT (Strict.tell w >> f a)

  ifte t th el = Strict.WriterT $ ifte
    (Strict.runWriterT t)
    (\(a,w) -> Strict.runWriterT (Strict.tell w >> th a))
    (Strict.runWriterT el)

  once ma = Strict.WriterT $ once (Strict.runWriterT ma)

instance (MonadLogic m, Monoid w) => MonadLogic (Lazy.WriterT w m) where
  msplit wm = Lazy.WriterT $ msplit (Lazy.runWriterT wm) >>= \case
    Nothing -> return (Nothing, mempty)
    Just ((a,w), m) -> return (Just (a, Lazy.WriterT m), w)

  interleave ma mb = Lazy.WriterT $
    Lazy.runWriterT ma `interleave` Lazy.runWriterT mb

  ma >>- f = Lazy.WriterT $
    Lazy.runWriterT ma >>- \(a,w) -> Lazy.runWriterT (Lazy.tell w >> f a)

  ifte t th el = Lazy.WriterT $ ifte
    (Lazy.runWriterT t)
    (\(a,w) -> Lazy.runWriterT (Lazy.tell w >> th a))
    (Lazy.runWriterT el)

  once ma = Lazy.WriterT $ once (Lazy.runWriterT ma)

-- |
-- @
-- msplit >=> reflect == m
-- @
reflect :: MonadLogic m => Maybe (a, m a) -> m a
reflect Nothing = mzero
reflect (Just (a, m)) = return a `mplus` m

lnot :: MonadLogic m => m a -> m ()
lnot m = ifte (once m) (const mzero) (return ())

newtype LogicT m a = LogicT
  { runLogicT :: forall r. (a -> m r -> m r) -> m r -> m r
  }

type Logic = LogicT Identity

runLogic :: Logic a -> forall r. (a -> r -> r) -> r -> r
runLogic l s f = runIdentity $ runLogicT l (fmap . s) (Identity f)

pattern Logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a
pattern Logic f <- (runLogic -> f) where
  Logic f = LogicT $ \k -> Identity .
                     f (\a -> runIdentity . k a . Identity) .
                     runIdentity

instance Functor (LogicT f) where
  fmap f lt = LogicT $ \sk fk -> runLogicT lt (sk . f) fk

instance Applicative (LogicT f) where
  pure a = LogicT $ \sk fk -> sk a fk
  f <*> a = LogicT $ \sk fk -> runLogicT f (\g fk' -> runLogicT a (sk . g) fk') fk

instance Alternative (LogicT f) where
  empty = LogicT $ \_ fk -> fk
  f1 <|> f2 = LogicT $ \sk fk -> runLogicT f1 sk (runLogicT f2 sk fk)

instance Monad (LogicT m) where
  return a = LogicT $ \sk fk -> sk a fk
  m >>= f = LogicT $ \sk fk -> runLogicT m (\a fk' -> runLogicT (f a) sk fk') fk
  fail _ = LogicT $ \_ fk -> fk

instance MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = LogicT $ \sk fk -> m >>= \a -> sk a fk

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit m = lift $ runLogicT m ssk (return Nothing) where
    ssk a fk = return $ Just (a, (lift fk >>= reflect))

instance (Monad m, Foldable m) => Foldable (LogicT m) where
  foldMap f m = fold $ runLogicT m (liftM . mappend . f) (return mempty)

instance Traversable (LogicT Identity) where
  traverse g l = runLogic l (\a ft -> cons <$> g a <*> ft) (pure mzero) where
    cons a l' = return a `mplus` l'

instance MonadReader r m => MonadReader r (LogicT m) where
  ask = lift ask
  local f m = LogicT $ \sk fk -> runLogicT m ((local f .) . sk) (local f fk)

instance MonadState s m => MonadState s (LogicT m) where
  get = lift get
  put = lift . put

instance MonadError e m => MonadError e (LogicT m) where
  throwError = lift . throwError
  catchError m h = LogicT $ \sk fk -> let
      handle r = r `catchError` \e -> runLogicT (h e) sk fk
    in handle $ runLogicT m (\a -> sk a . handle) fk

observe :: Logic a -> a
observe = runIdentity . observeT 

observeAll :: Logic a -> [a]
observeAll = runIdentity . observeAllT

observeMany :: Int -> Logic a -> [a]
observeMany i = runIdentity . observeManyT i

observeT :: Monad m => LogicT m a -> m a
observeT lt = runLogicT lt (const . return) (fail "No answer.")

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = runLogicT m (liftM . (:)) (return [])

observeManyT :: Monad m => Int -> LogicT m a -> m [a]
observeManyT n m
  | n <= 0 = return []
  | n == 1 = runLogicT m (\a _ -> return [a]) (return [])
  | otherwise = runLogicT (msplit m) sk (return []) where
    sk Nothing _ = return []
    sk (Just (a, m')) _ = (a:) `liftM` observeManyT (n-1) m'
