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
  , TermM
  , newTerm
  , find -- current root
  , is
  , isn't
  , decide
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Lens hiding (isn't)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Hashable
import Data.HashSet as HS
import Ref
import Unique

data Content s
  = Root
    {-# unpack #-} !Int -- rank
    !(HashSet (Term s)) -- roots of known-disjoint terms
  | Child !(Term s)     -- parent

data Term s = Term
  { equalityId       :: {-# unpack #-} !(Unique s)
  , equalityReference :: {-# unpack #-} !(Ref s (Content s))
  }

type TermM m = Term (PrimState m)

instance Eq (Term s) where
  (==) = (==) `on` equalityId

instance Hashable (Term s) where
  hash = hash . equalityId
  hashWithSalt d = hashWithSalt d . equalityId

instance Reference s (Content s) (Term s) where
  reference = equalityReference

newTerm :: MonadRef m => m (TermM m)
newTerm = Term <$> newUnique <*> newRef (Root 0 mempty)

-- returns rank and disjoint set as well as result
findEx :: MonadRef m => TermM m -> m (Int, HashSet (TermM m), TermM m)
findEx d = readRef d >>= \case
  Root i xs -> pure (i, xs, d)
  Child s -> do
    x <- findEx s
    x <$ writeRef d (Child $ x^._3)

find :: MonadRef m => TermM m -> m (TermM m)
find d = readRef d >>= \case
  Root{} -> pure d
  Child s -> do
    x <- find s
    x <$ writeRef d (Child x)

is :: MonadRef m => TermM m -> TermM m -> m ()
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

isn't :: MonadRef m => TermM m -> TermM m -> m ()
isn't m n = do
  (mrank,notm,mroot) <- findEx m
  (nrank,notn,nroot) <- findEx n
  guard $ mroot /= nroot
  writeRef mroot $ Root mrank $ HS.insert nroot notm
  writeRef nroot $ Root nrank $ HS.insert mroot notn

-- | ground out an equality relation
decide :: MonadRef m => TermM m -> TermM m -> m Bool
decide m n
    = True <$ is m n
  <|> False <$ isn't m n
