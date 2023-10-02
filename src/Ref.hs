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
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Type.Coercion
import Logic.Class
import Unsafe.Coerce

-- | Explicitly share a computation. This allows us to branch /now/ but perform
-- the computation /later/, only as we need the value.
--
-- This function obeys the following properties;
--
-- prop> memo (a `mplus` b) = memo a `mplus` memo b
-- prop> memo mzero = return mzero
-- prop> memo undefined = return undefined
--
-- Based on <http://www-ps.informatik.uni-kiel.de/~sebf/data/pub/icfp09.pdf>
-- section 5.3.2
memo :: MonadRef m => m a -> m (m a)
memo ma = do
  r <- newRef Nothing -- Maybe a
  pure $ readRef r >>= \case
    Nothing -> do
      a <- ma
      a <$ writeRef r (Just a)
    Just a -> pure a

-- | A 'MonadRef' is a 'Monad' which supports both references (via the 'PrimMonad' class) and
-- backtracking (via the 'MonadPlus' class).
type MonadRef m = (PrimMonad m, MonadLogic m)

-- | Morally, this brackets the success continuation with an undo operation to roll back with upon
-- taking the failure continuation. Users of this function should endeavour to guarantee that the
-- second argument does in fact undo the effects of the first.
unwind
  :: MonadRef m
  => (a -> (b, c))
  -> (c -> m d)
  -> m a
  -> m b
unwind f mu na = na >>= \a -> case f a of
  (b, c) -> pureWithCleanup (b :&&: void (mu c))

-- | Safely-backtracked 'MutVar's. The write operations on a 'Ref' will be reverted
-- upon a failure due to 'mzero'.
newtype Ref s a = Ref { getRef :: MutVar s a }
  deriving Eq

-- | Type synonym for 'Ref' parameterized by the monad rather than the state type.
type RefM m = Ref (PrimState m)

instance TestCoercion (Ref s) where
  testCoercion (Ref s :: Ref s a) (Ref t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

-- | An instance of 'Reference' can be thought of as a wrapper around 'Ref'.
class Reference s a t | t -> s a where
  reference :: t -> Ref s a

-- | Type synonym for 'Reference' parameterized by the monad rather than the state type.
type ReferenceM m = Reference (PrimState m)

instance Reference s a (Ref s a) where
  reference = id

-- | Create a new 'RefM' containing a given value.
newRef :: PrimMonad m => a -> m (RefM m a)
newRef = fmap Ref . newMutVar

-- | Create a new 'RefM' from a function which is passed the new reference itself.
-- This is useful for constructing cycles in a chain of references, for example.
newSelfRef :: PrimMonad m => (RefM m a -> a) -> m (RefM m a)
newSelfRef f = do
  x <- newMutVar undefined
  Ref x <$ writeMutVar x (f $ Ref x)

-- | Read the value from a reference.
readRef :: (PrimMonad m, ReferenceM m a t) => t -> m a
readRef = readMutVar . getRef . reference

-- | Write a value to a reference. This operation will 'unwind' upon a failure.
writeRef :: (MonadRef m, ReferenceM m a t) => t -> a -> m ()
writeRef (reference -> Ref r) a'
  = unwind ((),) (writeMutVar r) $ atomicModifyMutVar r (a',)

-- | Modify and extract a value from a reference in-place. This operation will 'unwind' upon
-- a failure.
updateRef :: (MonadRef m, ReferenceM m a t) => t -> (a -> (b, a)) -> m b
updateRef (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar r $ \a@(f->(b,a'))->(a',(b,a))

-- | Strictly modify and extract a value from a reference in-place. This operation will 'unwind'
-- upon a failure.
updateRef' :: (MonadRef m, ReferenceM m a t) => t -> (a -> (b, a)) -> m b
updateRef' (reference -> Ref r) f = unwind id (writeMutVar r) $ atomicModifyMutVar' r $ \a@(f->(b,a'))->(a',(b,a))

-- | Modify a reference in-place. This operation will 'unwind' upon a failure.
modifyRef :: (MonadRef m, ReferenceM m a t) => t -> (a -> a) -> m ()
modifyRef (reference -> Ref r) f = unwind ((),) (writeMutVar r) $ atomicModifyMutVar r $ \a -> (f a,a)

-- | Strictly modify a reference in-place. This operation will 'unwind' upon a failure.
modifyRef' :: (MonadRef m, ReferenceM m a t) => t -> (a -> a) -> m ()
modifyRef' (reference -> Ref r) f = unwind ((),) (writeMutVar r) $ atomicModifyMutVar' r $ \a -> (f a,a)
