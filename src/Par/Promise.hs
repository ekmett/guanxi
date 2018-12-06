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
import Control.Monad.State.Class
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Proxy
import Par.Class
import Ref.Signal
import Ref.Base

data Promise m a = Promise
  { _promiseVal :: Ref m (Maybe a)
  , _promiseSignal :: Signal m 
  }

-- | this promise does not require the promise to be fulfilled for the world to be valid
newPromise_ :: (MonadState s m, HasSignalEnv s m, PrimMonad m) => m (Promise m a)
newPromise_ = do
  r <- newRef Nothing
  Promise r <$> newSignal_

-- | Create a new promise that must be fulfilled for the world to be valid
newPromise :: (MonadState s m, HasSignalEnv s m, MonadRef m) => m (Promise m a)
newPromise = do
  r <- newRef Nothing
  Promise r <$> newSignal (\_ -> readRef r >>= guard . isJust)

-- | Fulfill a promise
fulfill :: (MonadState s m, HasSignalEnv s m, Eq a, MonadRef m) => Promise m a -> a -> m ()
fulfill (Promise r v) a = join $ updateRef r $ \case
  Nothing      -> (fire v, Just a)
  jb@(Just b)  -> (guard $ a == b, jb)

-- fulfill a promise, assumes that any attempts at multiple fulfillment used the same value
unsafeFulfill :: (MonadState s m, HasSignalEnv s m, MonadRef m) => Promise m a -> a -> m ()
unsafeFulfill (Promise r v) a = join $ updateRef r $ \case
  Nothing -> (fire v, Just a)
  rb      -> (pure (), rb)

-- | Demand that another inhabitant of this world fulfills this promise
demand :: forall m s a. (MonadPar m, MonadState s m, HasSignalEnv s m, MonadRef m) => Promise m a -> m a
demand (Promise r v) = callCC $ \k -> join $ updateRef r $ \case
  Nothing -> (propagate v (Proxy :: Proxy m) (readRef r >>= traverse_ k) *> halt, Nothing)
  ja@(Just a) -> (pure a, ja)
