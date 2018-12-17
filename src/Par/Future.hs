-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Par.Future
  ( Future
  , newFuture
  , await
  ) where

import Par.Class
import Par.Promise
import Signal

newtype Future m a = Future (Promise m a)

newFuture :: (MonadPar m, MonadSignal e m) => m a -> m (Future m a)
newFuture m = do
  p <- newPromise_
  fork $ do
    a <- m
    unsafeFulfill p a
  pure $ Future p

await :: (MonadPar m, MonadSignal e m) => Future m a -> m a
await (Future p) = demand p
