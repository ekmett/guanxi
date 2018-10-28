{-# language DefaultSignatures #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

module Key where

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
import Data.Type.Equality
import Data.Proxy
import Control.Monad.ST
import Unsafe.Coerce

class Equitable t where
  test :: t a -> t b -> Maybe (a :~: b)

instance Equitable ((:~:)c) where
  test Refl Refl = Just Refl

newtype PrimKey s a = PrimKey (MutVar s (Proxy a))

instance Equitable (PrimKey s) where
  test (PrimKey s) (PrimKey u) 
    | s == unsafeCoerce u = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  -- a potentially rather small trusted code base

-- safer than ST s in that we can transform it with things like LogicT
class (Equitable (Key m), Monad m) => MonadKey m where
  type Key m :: * -> *
  type Key m = PrimKey (PrimState m)
  newKey :: m (Key m a)
  default newKey :: (Key m ~ PrimKey (PrimState m), PrimMonad m) => m (Key m a)
  newKey = stToPrim $ PrimKey <$> newMutVar Proxy

instance MonadKey (ST s)
instance MonadKey IO

instance MonadKey m => MonadKey (Strict.StateT s m) where
  type Key (Strict.StateT s m) = Key m
  newKey = lift newKey

instance MonadKey m => MonadKey (Lazy.StateT s m) where
  type Key (Lazy.StateT s m) = Key m
  newKey = lift newKey

instance (Monoid w, MonadKey m) => MonadKey (Strict.WriterT w m) where
  type Key (Strict.WriterT w m) = Key m
  newKey = lift newKey

instance (Monoid w, MonadKey m) => MonadKey (Lazy.WriterT w m) where
  type Key (Lazy.WriterT w m) = Key m
  newKey = lift newKey

instance MonadKey m => MonadKey (ReaderT e m) where
  type Key (ReaderT e m) = Key m
  newKey = lift newKey

instance (Monoid w, MonadKey m) => MonadKey (Strict.RWST r w s m) where
  type Key (Strict.RWST r w s m) = Key m
  newKey = lift newKey

instance (Monoid w, MonadKey m) => MonadKey (Lazy.RWST r w s m) where
  type Key (Lazy.RWST r w s m) = Key m
  newKey = lift newKey

instance MonadKey m => MonadKey (ContT r m) where
  type Key (ContT r m) = Key m
  newKey = lift newKey

instance MonadKey m => MonadKey (ExceptT e m) where
  type Key (ExceptT e m) = Key m
  newKey = lift newKey
