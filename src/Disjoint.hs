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
-- disjoint set forests with path-compression providing
-- a theory of equality without disequality constraints
module Disjoint where

import Control.Applicative (liftA2)
import Control.Monad.Primitive
import Ref

data Content s
  = Root {-# unpack #-} !Int
  | Child {-# unpack #-} !(Disjoint s)

newtype Disjoint s = Disjoint { getDisjoint :: Ref s (Content s) }
  deriving Eq

type DisjointM m = Disjoint (PrimState m)

instance Reference s (Content s) (Disjoint s) where
  reference = getDisjoint

newDisjoint :: MonadRef m => m (DisjointM m)
newDisjoint = Disjoint <$> newRef (Root 0)

-- return rank as well as result
findEx :: MonadRef m => DisjointM m -> m (Int, DisjointM m)
findEx d = readRef d >>= \case
  Root i -> pure (i, d)
  Child s -> do
    x <- findEx s
    x <$ writeRef d (Child $ snd x)

find :: MonadRef m => DisjointM m -> m (DisjointM m)
find d = readRef d >>= \case
  Root _ -> pure d
  Child s -> do
    x <- find s
    x <$ writeRef d (Child x)

union :: MonadRef m => DisjointM m -> DisjointM m -> m ()
union m n = do
  (mrank,mroot) <- findEx m
  (nrank,nroot) <- findEx n
  case compare mrank nrank of
    LT -> writeRef mroot $ Child nroot
    GT -> writeRef nroot $ Child mroot
    EQ -> do
      writeRef mroot $ Child nroot
      writeRef nroot $ Root (nrank+1)

-- | check if currently equal
eq :: MonadRef m => DisjointM m -> DisjointM m -> m Bool
eq m n = liftA2 (==) (find m) (find n)
