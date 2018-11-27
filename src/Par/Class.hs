{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}

module Par.Class
  ( MonadPar(..)
  ) where

import Control.Monad.Cont.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

class MonadCont m => MonadPar m where
  fork :: m () -> m ()

  halt :: m a
  default halt :: (m ~ t n, MonadTrans t, MonadPar n) => m a 
  halt = lift halt

  yield :: m ()
  default yield :: (m ~ t n, MonadTrans t, MonadPar n) => m ()
  yield = lift yield
 
instance MonadPar m => MonadPar (ReaderT e m) where
  fork (ReaderT f) = ReaderT (fork . f)
