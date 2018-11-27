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
