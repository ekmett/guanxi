{-# language LambdaCase #-}
{-# language FlexibleContexts #-}

module Promise 
  ( Promise
  , newPromise
  , newPromise_
  , demand
  , (!=)
  , (!!=)
  ) where

import Cell
import Control.Applicative
import Control.Lens
import Control.Monad (guard, join)
import Control.Monad.Cont.Class
import Control.Monad.State.Class
import Data.Either
import Key
import Ref

data Promise m a = Promise
  { _promiseVal :: Ref (KeyState m) (Either (a -> m ()) a)
  , _promiseVar :: Var m 
  }

instance HasCellIds (Promise m a) where
  cellIds = cellIds . _promiseVar

-- | this promise does not require the promise to be fulfilled for the world to be valid
newPromise_ :: (MonadState s m, HasCellEnv s m, MonadKey m) => m (Promise m a)
newPromise_ = do
  r <- newRef $ Left $ const $ pure ()
  Promise r <$> newVar_

-- | Create a new promise that must be fulfilled for the world to be valid
newPromise :: (MonadState s m, HasCellEnv s m, MonadKey m, Alternative m) => m (Promise m a)
newPromise = do
  r <- newRef $ Left $ const $ pure ()
  Promise r <$> newVar (\_ -> readRef r >>= guard . isRight)

-- | Fulfill a promise
(!=) :: (MonadState s m, HasCellEnv s m, Eq a, Alternative m) => Promise m a -> a -> m ()
Promise r v != a = join $ ref r %%= \case
  Left e -> (fire v *> e a, Right a)
  rb@(Right b) -> (guard $ a == b, rb)

-- fulfill a promise, assumes that any attempts at multiple fulfillment used the same value
(!!=) :: (MonadState s m, HasCellEnv s m) => Promise m a -> a -> m ()
Promise r v !!= a = join $ ref r %%= \case
  Left e -> (fire v *> e a, Right a)
  rb -> (pure (), rb)

-- | Demand that another inhabitant of this world fulfills this promise
demand :: (MonadCont m, MonadState s m, HasRefEnv s (KeyState m), Alternative m) => Promise m a -> m a
demand (Promise r _) = callCC $ \k ->
  join $ ref r %%= \case
    Left e -> (empty, Left $ \a -> e a *> k a)
    ra@(Right a) -> (pure a, ra)
