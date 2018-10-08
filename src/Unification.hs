{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unification where

import Control.Monad
import Data.IORef
import Data.Kind
import Freer
import Unified

class EqVar (v :: (Type -> Type) -> Type) where
  eqVar :: v f -> v f -> Bool

class (MonadPlus m, EqVar v) =>
      MonadUnify v m
  | m -> v
  where
  newVar :: m (v f)
  readVar :: v f -> m (Maybe (Free f (v f)))
  writeVar :: v f -> Free f (v f) -> m ()

newtype IOVar f = IOVar
  { runIOVar :: IORef (Maybe (Free f (IOVar f)))
  } deriving (Eq)

instance EqVar IOVar where
  eqVar = (==)

instance MonadUnify IOVar IO where
  newVar = IOVar <$> newIORef Nothing
  readVar = readIORef . runIOVar
  writeVar v = writeIORef (runIOVar v) . Just

unifyVar ::
     (MonadUnify v m, Unified f) => v f -> Free f (v f) -> m (Free f (v f))
unifyVar a x =
  readVar a >>= \case
    Nothing -> do
      x' <- zonk x
      guard $ not $ any (eqVar a) x'
      x' <$ writeVar a x'
    Just y -> do
      y' <- unify x y
      y' <$ writeVar a y'

unify ::
     forall m v f. (MonadUnify v m, Unified f)
  => Free f (v f)
  -> Free f (v f)
  -> m (Free f (v f))
unify l r = go l (view l) (view r)
  where
    go ::
         Free f (v f)
      -> FreeView f (v f)
      -> FreeView f (v f)
      -> m (Free f (v f))
    go t (Pure v) (Pure u)
      | eqVar v u = return t
    go _ (Pure a) y = unifyVar a (unview y)
    go t _ (Pure a) = unifyVar a t
    go _ (Free xs kx) (Free ys ky) =
      free <$> merge (\x y -> unify (kx x) (ky y)) xs ys

-- | zonk/walk-flatten
zonk :: (MonadUnify v m, Traversable f) => Free f (v f) -> m (Free f (v f))
zonk = fmap join . traverse go
  where
    go ::
         forall (m :: Type -> Type) (v :: (Type -> Type) -> Type) (f :: Type -> Type).
         (MonadUnify v m, Traversable f)
      => v f
      -> m (Free f (v f))
    go v =
      readVar v >>= \case
        Nothing -> return $ pure v
        Just t -> do
          t' <- zonk t
          t' <$ writeVar v t' -- path compression
