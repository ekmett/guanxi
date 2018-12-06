{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}

module Back.Class where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.RWS.Strict as Strict
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.ST
import Control.Monad.State.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy

class PrimMonad m => MonadBack m where
  backtrackWith :: ST (PrimState m) () -> m ()
  default backtrackWith :: (MonadTrans t, MonadBack n, m ~ t n, PrimState m ~ PrimState n) => ST (PrimState m) () -> m ()
  backtrackWith = lift . backtrackWith

instance MonadBack m => MonadBack (ReaderT r m)
instance MonadBack m => MonadBack (Strict.StateT s m)
instance MonadBack m => MonadBack (Lazy.StateT s m)
instance (MonadBack m, Monoid w) => MonadBack (Strict.WriterT w m)
instance (MonadBack m, Monoid w) => MonadBack (Lazy.WriterT w m)
instance (MonadBack m, Monoid w) => MonadBack (Strict.RWST r w s m)
instance (MonadBack m, Monoid w) => MonadBack (Lazy.RWST r w s m)

-- | morally, this brackets the success continuation with an undo operation to roll back with upon
-- taking the failure continuation
unwind
  :: MonadBack m
  => (a -> (b, c))
  -> (c -> ST (PrimState m) ())
  -> m a
  -> m b
unwind f mu na = na >>= \a -> case f a of
  (b, c) -> b <$ backtrackWith (mu c)
