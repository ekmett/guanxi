{-# language ViewPatterns #-}
{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language Trustworthy #-}

module Ref.Base
  ( memo
  , unwind
  , MonadRef, Ref, Reference(..)
  , newRef, newSelfRef
  , readRef, writeRef
  , updateRef, updateRef'
  , modifyRef, modifyRef'
  ) where

import Control.Applicative
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Type.Coercion
import Unsafe.Coerce

-- | Based on <http://www-ps.informatik.uni-kiel.de/~sebf/data/pub/icfp09.pdf>
-- section 5.3.2
memo :: MonadRef m => m a -> m (m a)
memo ma = do
  r <- newRef Nothing -- Either (m a) a
  pure $ readRef r >>= \case
    Nothing -> do
      a <- ma
      a <$ writeRef r (Just a)
    Just a -> pure a

type MonadRef m = (PrimMonad m, Alternative m)

-- | morally, this brackets the success continuation with an undo operation to roll back with upon
-- taking the failure continuation
unwind
  :: MonadRef m 
  => (a -> (b, c))
  -> (c -> m d)
  -> m a
  -> m b
unwind f mu na = na >>= \a -> case f a of
  (b, c) -> pure b <|> (mu c *> empty)

-- | safely-backtracked mutvars
newtype Ref m a = Ref { getRef :: MutVar (PrimState m) a }

instance TestCoercion (Ref m) where
  testCoercion (Ref s :: Ref m a) (Ref t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

class Reference m a t | t -> m a where
  reference :: t -> Ref m a

instance Reference m a (Ref m a) where
  reference = id

newRef :: PrimMonad m => a -> m (Ref m a)
newRef = fmap Ref . newMutVar

newSelfRef :: PrimMonad m => (Ref m a -> a) -> m (Ref m a)
newSelfRef f = do
  x <- newMutVar undefined
  Ref x <$ writeMutVar x (f $ Ref x)

readRef :: (PrimMonad m, Reference m a t) => t -> m a
readRef = readMutVar . getRef . reference

writeRef :: (MonadRef m, Reference m a t) => t -> a -> m ()
writeRef (reference -> Ref r) a' = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \ a -> (a', a)

updateRef :: (MonadRef m, Reference m a t) => t -> (a -> (b, a)) -> m b
updateRef (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar r $ \a@(f->(b,a'))->(a',(b,a))

updateRef' :: (MonadRef m, Reference m a t) => t -> (a -> (b, a)) -> m b
updateRef' (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar' r $ \a@(f->(b,a'))->(a',(b,a))

modifyRef :: (MonadRef m, Reference m a t) => t -> (a -> a) -> m ()
modifyRef (reference -> Ref r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \a -> (f a,a)

modifyRef' :: (MonadRef m, Reference m a t) => t -> (a -> a) -> m ()
modifyRef' (reference -> Ref r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar' r $ \a -> (f a,a)

