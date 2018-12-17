{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

module Par.Promise
  ( Promise
  , newPromise
  , newPromise_
  , demand
  , fulfill
  , unsafeFulfill
  ) where

import Control.Monad (guard, join)
import Control.Monad.Cont.Class
import Control.Monad.Primitive
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Proxy
import Par.Class
import Ref
import Signal

data Promise m a = Promise
  { _promiseVal :: RefM m (Maybe a)
  , _promiseSignal :: Signal m
  }

-- | this promise does not require the promise to be fulfilled for the world to be valid
newPromise_ :: PrimMonad m => m (Promise m a)
newPromise_ = do
  r <- newRef Nothing
  Promise r <$> newSignal_

-- | Create a new promise that must be fulfilled for the world to be valid
newPromise :: MonadSignal e m => m (Promise m a)
newPromise = do
  r <- newRef Nothing
  Promise r <$> newSignal (\_ -> readRef r >>= guard . isJust)

-- | Fulfill a promise
fulfill :: (MonadSignal e m, Eq a) => Promise m a -> a -> m ()
fulfill (Promise r v) a = join $ updateRef r $ \case
  Nothing      -> (fire v, Just a)
  jb@(Just b)  -> (guard $ a == b, jb)

-- fulfill a promise, assumes that any attempts at multiple fulfillment used the same value
unsafeFulfill :: MonadSignal e m => Promise m a -> a -> m ()
unsafeFulfill (Promise r v) a = join $ updateRef r $ \case
  Nothing -> (fire v, Just a)
  rb      -> (pure (), rb)

-- | Demand that another inhabitant of this world fulfills this promise
demand :: forall m e a. (MonadPar m, MonadSignal e m) => Promise m a -> m a
demand (Promise r v) = callCC $ \k -> join $ updateRef r $ \case
  Nothing -> (propagate v (Proxy :: Proxy m) (readRef r >>= traverse_ k) *> halt, Nothing)
  ja@(Just a) -> (pure a, ja)
