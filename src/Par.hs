{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module Par where

import Control.Monad hiding (fail)
import Control.Monad.Cont hiding (fail) -- fix this API!
import Control.Monad.Fail
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Applicative
import Control.Lens hiding (Empty, snoc, uncons)
import Prelude hiding (fail)
import Unaligned

newtype ParEnv m = ParEnv { _todo :: Q (m ()) }

makeClassy ''ParEnv

newtype Par m a = ParT { runParT :: ContT () m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadCont)

{-# complete Par #-}

pattern Par :: ((a -> m ()) -> m ()) -> Par m a
pattern Par m = ParT (ContT m)

deriving instance MonadState s m => MonadState s (Par m)
deriving instance MonadReader e m => MonadReader e (Par m)

yield :: (MonadState s m, HasParEnv s m) => Par m ()
yield = Par $ \k ->
  join $ todo %%= \case
    Cons x xs -> (x, snoc xs (k ()))
    Nil -> (k (), Nil)

fork :: (MonadState s m, HasParEnv s m, Alternative m) => Par m () -> Par m ()
fork (Par m) = lift $
  todo %= \ys -> snoc ys $ m $ \_ -> join $ todo %%= \case
    Cons x xs -> (x, xs)
    Nil -> (empty, Nil)

instance (MonadFail m, MonadState s m, HasParEnv s m) => MonadFail (Par m) where
  fail s = Par $ \_ -> 
    join $ todo %%= \xss -> case uncons xss of
      Empty    -> (fail s, xss)
      x :&: xs -> (x, xs)

instance
  ( MonadState s m
  , HasParEnv s m
  , Alternative m
  ) => Alternative (Par m) where

  empty = Par $ \_ -> 
    join $ todo %%= \xss -> case uncons xss of
      Empty    -> (empty, xss)
      x :&: xs -> (x, xs)

  Par m <|> Par n = Par $ \k -> m k <|> n k

instance
  ( MonadState s m
  , HasParEnv s m
  , Alternative m
  ) => MonadPlus (Par m) where
  mzero = empty
  mplus = (<|>)
