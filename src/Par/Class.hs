{-# language DefaultSignatures #-}
{-# language TypeFamilies #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

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
