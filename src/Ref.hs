{-# language ViewPatterns #-}
{-# language ConstraintKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language Trustworthy #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}

module Ref
  ( memo
  , unwind
  , MonadRef, Ref, RefM, Reference(..), ReferenceM
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
  r <- newRef Nothing -- Maybe a
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
newtype Ref s a = Ref { getRef :: MutVar s a }
  deriving Eq

type RefM m = Ref (PrimState m)

instance TestCoercion (Ref s) where
  testCoercion (Ref s :: Ref s a) (Ref t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

class Reference s a t | t -> s a where
  reference :: t -> Ref s a

type ReferenceM m = Reference (PrimState m)

instance Reference s a (Ref s a) where
  reference = id

newRef :: PrimMonad m => a -> m (RefM m a)
newRef = fmap Ref . newMutVar

newSelfRef :: PrimMonad m => (RefM m a -> a) -> m (RefM m a)
newSelfRef f = do
  x <- newMutVar undefined
  Ref x <$ writeMutVar x (f $ Ref x)

readRef :: (PrimMonad m, ReferenceM m a t) => t -> m a
readRef = readMutVar . getRef . reference

writeRef :: (MonadRef m, ReferenceM m a t) => t -> a -> m ()
writeRef (reference -> Ref r) a'
  = unwind ((),) (writeMutVar r) $ atomicModifyMutVar r (a',)

updateRef :: (MonadRef m, ReferenceM m a t) => t -> (a -> (b, a)) -> m b
updateRef (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar r $ \a@(f->(b,a'))->(a',(b,a))

updateRef' :: (MonadRef m, ReferenceM m a t) => t -> (a -> (b, a)) -> m b
updateRef' (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar' r $ \a@(f->(b,a'))->(a',(b,a))

modifyRef :: (MonadRef m, ReferenceM m a t) => t -> (a -> a) -> m ()
modifyRef (reference -> Ref r) f = unwind ((),) (writeMutVar r) $ atomicModifyMutVar r $ \a -> (f a,a)

modifyRef' :: (MonadRef m, ReferenceM m a t) => t -> (a -> a) -> m ()
modifyRef' (reference -> Ref r) f = unwind ((),) (writeMutVar r) $ atomicModifyMutVar' r $ \a -> (f a,a)
