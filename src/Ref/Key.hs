{-# language DefaultSignatures #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language RoleAnnotations #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This construction is based on
-- <https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf The Key Monad: Type-Safe Unconstrained Dynamic Typing>
-- by Atze van der Ploeg, Koen Claessen, and Pablo Buiras

module Ref.Key 
  ( Key
  , Box(..)
  , unlock
  , MonadKey(..)
  , primKey
  , runKey
  ) where

import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Reader as Lazy
import Control.Monad.Trans.Except
import Data.Primitive.MutVar
import Data.Type.Coercion
import Data.Type.Equality
import Data.Proxy
import Control.Monad.ST
import Unsafe.Coerce

newtype Key s a = Key (MutVar s (Proxy a))
  deriving Eq

type role Key nominal nominal

instance TestEquality (Key s) where
  -- testEquality :: Key s a -> Key s b -> Maybe (a :~: b)
  testEquality (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  {-# inline testEquality #-}

instance TestCoercion (Key s) where
  testCoercion (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

-- safer than ST s in that we can transform it with things like LogicT
class Monad m => MonadKey m where
  type KeyState m :: *
  type KeyState m = PrimState m
  -- TODO: we can't use PrimState for this it is class associated
  newKey :: m (Key (KeyState m) a)
  default newKey
    :: (m ~ t n
       , MonadTrans t, MonadKey n
       , KeyState m ~ KeyState n
       ) => m (Key (KeyState m) a)
  newKey = lift newKey
  {-# inline newKey #-}

primKey :: PrimMonad m => m (Key (PrimState m) a)
primKey = stToPrim $ Key <$> newMutVar Proxy
{-# inline primKey #-}

runKey :: (forall m. MonadKey m => m a) -> a
runKey s = runST s

instance MonadKey (ST s) where newKey = primKey
instance MonadKey IO where newKey = primKey

instance MonadKey m => MonadKey (Strict.StateT s m) where
  type KeyState (Strict.StateT s m) = KeyState m

instance MonadKey m => MonadKey (Lazy.StateT s m) where
  type KeyState (Lazy.StateT s m) = KeyState m
instance (Monoid w, MonadKey m) => MonadKey (Strict.WriterT w m) where
  type KeyState (Strict.WriterT w m) = KeyState m

instance (Monoid w, MonadKey m) => MonadKey (Lazy.WriterT w m) where
  type KeyState (Lazy.WriterT w m) = KeyState m

instance MonadKey m => MonadKey (ReaderT e m) where
  type KeyState (ReaderT e m) = KeyState m

instance (Monoid w, MonadKey m) => MonadKey (Strict.RWST r w s m) where
  type KeyState (Strict.RWST r w s m) = KeyState m

instance (Monoid w, MonadKey m) => MonadKey (Lazy.RWST r w s m) where
  type KeyState (Lazy.RWST r w s m) = KeyState m

instance MonadKey m => MonadKey (ContT r m) where
  type KeyState (ContT r m) = KeyState m

instance MonadKey m => MonadKey (ExceptT e m) where
  type KeyState (ExceptT e m) = KeyState m

data Box s where
  Lock :: {-# unpack #-} !(Key s a) -> a -> Box s

unlock :: Key s a -> Box s -> Maybe a
unlock k (Lock l x) = case testEquality k l of
  Just Refl -> Just x
  Nothing -> Nothing
{-# inline unlock #-}
