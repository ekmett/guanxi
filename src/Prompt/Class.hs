{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

module Prompt.Class where

import Control.Category
import Control.Monad
import Control.Monad.Reader
import Data.Kind
import Data.Type.Coercion
import Data.Type.Equality
import Prelude hiding ((.),id)

-- delimited continuations
class
  ( Monad m
  , Category (Sub m)
  , TestCoercion (Prompt m)
  , TestEquality (Prompt m)
  ) => MonadPrompt m where

  type Prompt m :: Type -> Type
  type Sub m :: Type -> Type -> Type

  newPrompt  :: m (Prompt m a)
  pushPrompt :: Prompt m a -> m a -> m a
  withSub :: Prompt m b -> (Sub m a b -> m b) -> m a
  pushSub :: Sub m a b -> m a -> m b

reset :: MonadPrompt m => (Prompt m a -> m a) -> m a
reset e = do
  p <- newPrompt
  pushPrompt p (e p)

shift, shift0, control, control0 :: MonadPrompt m => Prompt m b -> ((a -> m b) -> m b) -> m a
shift p f    = withSub p $ \sk -> pushPrompt p $ f $ pushPrompt p . pushSub sk . return
shift0 p f   = withSub p $ \sk -> f $ pushPrompt p . pushSub sk . return
control p f  = withSub p $ \sk -> pushPrompt p $ f $ pushSub sk . return
control0 p f = withSub p $ \sk -> f $ pushSub sk . return

abort :: MonadPrompt m => Prompt m b -> m b -> m a
abort p = withSub p . const

-- in the style of CC-delcont
shiftM, shiftM0, controlM, controlM0 :: MonadPrompt m => Prompt m b -> ((m a -> m b) -> m b) -> m a
shiftM p f    = withSub p $ \sk -> pushPrompt p $ f $ pushPrompt p . pushSub sk
shiftM0 p f   = withSub p $ \sk -> f $ pushPrompt p . pushSub sk
controlM p f  = withSub p $ pushPrompt p . f . pushSub
controlM0 p f = withSub p $ f . pushSub

instance MonadPrompt m => MonadPrompt (ReaderT e m) where
  type Prompt (ReaderT e m) = Prompt m
  type Sub (ReaderT e m) = Sub m
  newPrompt = lift newPrompt
  pushPrompt = mapReaderT . pushPrompt
  withSub p f = ReaderT $ \e -> withSub p $ \s -> runReaderT (f s) e
  pushSub = mapReaderT . pushSub
