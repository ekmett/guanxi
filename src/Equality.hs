{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- a simple theory of equality/inequality without congruence closure

module Equality
  ( Term
  , newTerm
  , find -- current root
  , is
  , isn't
  , decide
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (isn't)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Hashable
import Data.HashSet as HS
import Ref
import Unique

data Content m
  = Root
    {-# unpack #-} !Int -- rank
    !(HashSet (Term m)) -- roots of known-disjoint terms
  | Child !(Term m)     -- parent

data Term m = Term
  { equalityId       :: {-# unpack #-} !(Unique m)
  , equalityReference :: {-# unpack #-} !(Ref m (Content m))
  }

instance Eq (Term m) where
  (==) = (==) `on` equalityId

instance Hashable (Term m) where
  hash = hash . equalityId
  hashWithSalt d = hashWithSalt d . equalityId

instance Reference m (Content m) (Term m) where
  reference = equalityReference

newTerm :: MonadRef m => m (Term m)
newTerm = Term <$> newUnique <*> newRef (Root 0 mempty)

-- returns rank and disjoint set as well as result
findEx :: MonadRef m => Term m -> m (Int, HashSet (Term m), Term m)
findEx d = readRef d >>= \case
  Root i xs -> pure (i, xs, d)
  Child s -> do
    x <- findEx s
    x <$ writeRef d (Child $ x^._3)

find :: MonadRef m => Term m -> m (Term m)
find d = readRef d >>= \case
  Root{} -> pure d
  Child s -> do
    x <- find s
    x <$ writeRef d (Child x)

is :: (MonadRef m, Alternative m) => Term m -> Term m -> m ()
is m n = do
  (mrank,notm,mroot) <- findEx m
  (nrank,notn,nroot) <- findEx n
  guard $ not $ member mroot notn
  case compare mrank nrank of
    LT -> do
      writeRef mroot $ Child nroot
      for_ notm $ \i -> modifyRef' i $ \(Root irank noti) -> Root irank $ HS.insert nroot $ HS.delete mroot noti
      writeRef nroot $ Root nrank $ notm <> notn
    GT -> do
      writeRef nroot $ Child mroot
      for_ notn $ \i -> modifyRef' i $ \(Root irank noti) -> Root irank $ HS.insert mroot $ HS.delete nroot noti
      writeRef mroot $ Root mrank $ notm <> notm
    EQ -> do
      writeRef mroot $ Child nroot
      for_ notm $ \i -> modifyRef' i $ \(Root irank noti) -> Root irank $ HS.insert nroot $ HS.delete mroot noti
      writeRef nroot $ Root (nrank+1) $ notm <> notn

isn't :: (MonadRef m, Alternative m) => Term m -> Term m -> m ()
isn't m n = do
  (mrank,notm,mroot) <- findEx m
  (nrank,notn,nroot) <- findEx n
  guard $ mroot /= nroot
  writeRef mroot $ Root mrank $ HS.insert nroot notm
  writeRef nroot $ Root nrank $ HS.insert mroot notn

-- | ground out an equality relation
decide :: (MonadRef m, Alternative m) => Term m -> Term m -> m Bool
decide m n
    = True <$ is m n
  <|> False <$ isn't m n
