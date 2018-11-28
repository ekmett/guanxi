{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Logic.Class where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Unaligned.Base

class MonadPlus m => MonadLogic m where
  -- |
  -- @
  -- msplit empty ≡ pure Empty
  -- msplit (pure a <|> m) == pure (a :&: m)
  -- @
  msplit :: m a -> m (View a (m a)) 
  
  -- | fair disjunction
  interleave :: m a -> m a -> m a
  interleave m1 m2 = msplit m1 >>= \case
    Empty -> m2
    a :&: m1' -> return a `mplus` interleave m2 m1'

  -- | fair conjunction
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = do
    (a, m') <- msplit m >>= \case
      Empty -> mzero
      a :&: m' -> return (a, m')
    interleave (f a) (m' >>- f)

  -- |
  -- @
  -- ifte (return a) th el           == th a
  -- ifte mzero th el                == el
  -- ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
  -- @
  ifte :: m a -> (a -> m b) -> m b -> m b
  ifte t th el = msplit t >>= \case
    Empty -> el 
    a :&: m -> th a <|> (m >>= th)

  -- | pruning
  once :: m a -> m a
  once m = msplit m >>= \case
    Empty   -> empty
    a :&: _ -> pure a

instance MonadLogic [] where
  msplit []     = return Empty
  msplit (x:xs) = return $ x :&: xs

instance MonadLogic m => MonadLogic (ReaderT e m) where
  msplit rm = ReaderT $ \e -> msplit (runReaderT rm e) >>= \case
    Empty -> return Empty
    a :&: m -> return (a :&: lift m)

instance MonadLogic m => MonadLogic (Strict.StateT s m) where
  msplit sm = Strict.StateT $ \s -> msplit (Strict.runStateT sm s) >>= \case
    Empty -> return (Empty, s)
    (a,s') :&: m -> return (a :&: Strict.StateT (\_ -> m), s')

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
    Empty -> return (Empty , s)
    (a,s') :&: m -> return (a :&: Lazy.StateT (\_ -> m), s')

  interleave ma mb = Lazy.StateT $ \s -> Lazy.runStateT ma s `interleave` Lazy.runStateT mb s

  ma >>- f = Lazy.StateT $ \s -> Lazy.runStateT ma s >>- \(a,s') -> Lazy.runStateT (f a) s'

  ifte t th el = Lazy.StateT $ \s -> ifte
    (Lazy.runStateT t s)
    (\(a,s') -> Lazy.runStateT (th a) s')
    (Lazy.runStateT el s)

  once ma = Lazy.StateT $ \s -> once (Lazy.runStateT ma s)

instance (MonadLogic m, Monoid w) => MonadLogic (Strict.WriterT w m) where
  msplit wm = Strict.WriterT $ msplit (Strict.runWriterT wm) >>= \case
    Empty -> return (Empty, mempty)
    (a,w) :&: m -> return (a :&: Strict.WriterT m, w)

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
    Empty -> return (Empty , mempty)
    (a,w) :&: m -> return (a :&: Lazy.WriterT m, w)

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
-- msplit >=> reflect ≡ id
-- @
reflect :: Alternative m => View a (m a) -> m a
reflect Empty = empty
reflect (a :&: m) = pure a <|> m

lnot :: MonadLogic m => m a -> m ()
lnot m = ifte (once m) (const mzero) (return ())

