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
import Data.Foldable (traverse_)
import Data.Maybe
import Par.Class
import Key
import Ref

data Promise m a = Promise
  { _promiseVal :: Ref (KeyState m) (Maybe a)
  , _promiseVar :: Var m 
  }

-- | this promise does not require the promise to be fulfilled for the world to be valid
newPromise_ :: (MonadState s m, HasCellEnv s m, MonadKey m) => m (Promise m a)
newPromise_ = do
  r <- newRef Nothing
  Promise r <$> newVar_

-- | Create a new promise that must be fulfilled for the world to be valid
newPromise :: (MonadState s m, HasCellEnv s m, MonadKey m, Alternative m) => m (Promise m a)
newPromise = do
  r <- newRef Nothing
  Promise r <$> newVar (\_ -> readRef r >>= guard . isJust)

-- | Fulfill a promise
(!=) :: (MonadState s m, HasCellEnv s m, Eq a, Alternative m) => Promise m a -> a -> m ()
Promise r v != a = join $ ref r %%= \case
  Nothing      -> (fire v, Just a)
  jb@(Just b)  -> (guard $ a == b, jb)

-- fulfill a promise, assumes that any attempts at multiple fulfillment used the same value
(!!=) :: (MonadState s m, HasCellEnv s m) => Promise m a -> a -> m ()
Promise r v !!= a = join $ ref r %%= \case
  Nothing -> (fire v, Just a)
  rb      -> (pure (), rb)

-- | Demand that another inhabitant of this world fulfills this promise
demand :: (MonadPar m, MonadState s m, HasCellEnv s m, Alternative m) => Promise m a -> m a
demand (Promise r v) = callCC $ \k -> join $ ref r %%= \case
  Nothing     -> (newPropagator_ v () (readRef r >>= traverse_ k) *> halt, Nothing)
  ja@(Just a) -> (pure a, ja)
