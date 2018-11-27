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
{-# language TypeFamilies #-}

module Par
  ( ParEnv(..), HasParEnv(..)
  , Par(Par), runPar
  ) where

import Cell
import Control.Monad hiding (fail)
import Control.Monad.Cont hiding (fail) -- fix this API!
import Control.Monad.Fail
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Applicative
import Control.Lens hiding (Empty, snoc, uncons)
import Key
import Logic.Class
import Par.Class
import Prelude hiding (fail)
import Ref
import Unaligned

newtype ParEnv m = ParEnv { _todo :: Q (m ()) }

makeClassy ''ParEnv

newtype Par m a = ParT (ContT () m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadCont)

{-# complete Par #-}

instance MonadKey m => MonadKey (Par m) where
  type KeyState (Par m) = KeyState m
  newKey = lift newKey

instance PrimMonad m => PrimMonad (Par m) where
  type PrimState (Par m) = PrimState m
  primitive f = lift (primitive f)

pattern Par :: ((a -> m ()) -> m ()) -> Par m a
pattern Par m = ParT (ContT m)

runPar :: Par m a -> (a -> m ()) -> m ()
runPar (Par m) = m

deriving instance MonadState s m => MonadState s (Par m)
deriving instance MonadReader e m => MonadReader e (Par m)

instance (MonadState s m, HasParEnv s m, HasCellEnv s m, MonadLogic m, MonadKey m) => MonadLogic (Par m) where
  -- msplit m = Par $ \k -> runPar m (\a -> k (a :&: m)) <|> k Empty  -- uses the wrong (<|>)
  msplit m = Par $ \k -> do
    fired <- newRef False
    runPar m $ \a -> do
      ref fired .= True
      k (a :&: m)
    b <- readRef fired 
    unless b $ k Empty

-- halt and catch fire
hcf :: (MonadState s m, HasParEnv s m) => m ()
hcf = join $ todo %%= \xss -> case uncons xss of
  x :&: xs -> (x, xs)
  Empty -> (pure (), Nil) -- everybody is happy in this universe, it can now end
  
instance (MonadState s m, HasParEnv s m) => MonadPar (Par m) where
  yield = Par $ \k ->
    join $ todo %%= \xss -> case uncons xss of
      x :&: xs -> (x, xs `snoc` k ())
      Empty    -> (k (), Nil)

  halt = Par $ \_ -> hcf
  fork m = lift $ todo %= \ys -> snoc ys $ runPar m $ \_ -> hcf

instance (MonadFail m, MonadState s m, HasParEnv s m) => MonadFail (Par m) where
  fail s = Par $ \_ -> fail s

instance
  ( MonadState s m
  , HasParEnv s m
  , Alternative m
  ) => Alternative (Par m) where
  empty = Par $ \_ -> empty
  Par m <|> Par n = Par $ \k -> m k <|> n k

instance
  ( MonadState s m
  , HasParEnv s m
  , Alternative m
  ) => MonadPlus (Par m) where
  mzero = empty
  mplus = (<|>)
