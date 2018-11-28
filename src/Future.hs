-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Future 
  ( Future
  , newFuture
  , await
  ) where

import Cell
import Control.Applicative
import Control.Monad.State
import Key
import Par.Class
import Promise

newtype Future m a = Future (Promise m a)

newFuture :: (MonadPar m, MonadState s m, HasCellEnv s m, MonadKey m) => m a -> m (Future m a)
newFuture m = do
  p <- newPromise_
  Future p <$ do
    a <- m
    p !!= a

await :: (MonadPar m, MonadState s m, HasCellEnv s m, Alternative m) => Future m a -> m a
await (Future p) = Promise.demand p
