{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Domain.Relational where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Ref

-- Use path compression, union by size, with a big relational blob at the center
data Content s a
  = Child {-# unpack #-} !Int {-# unpack #-} !(Rel s a)
  | Root {-# unpack #-} !Int a

newtype Rel s a = Rel
  { domainReference :: Ref s (Content s a)
  } deriving Eq

type ContentM m = Content (PrimState m)
type RelM m = Rel (PrimState m)

instance Reference s (Content s a) (Rel s a) where
  reference = domainReference

findEx :: MonadRef m => RelM m a -> m (Int, Int, a, RelM m a)
findEx = go 0 where
  go !i m = readRef m >>= \case
    Root n a -> pure (i, n, a, m)
    Child j m' -> do
      result <- go (i + j) m'
      writeRef m $ Child (result^._1) (result^._4)
      return result

find :: MonadRef m => RelM m a -> m (RelM m a)
find = fmap snd . go 0 where
  go !i m = readRef m >>= \case
    Root{} -> pure (i, m)
    Child j m' -> do
      result <- go (i + j) m'
      writeRef m $ uncurry Child result
      return result

class Relational m a where
  -- merge disjoint relational domains
  disjointUnion :: Int -> a -> Int -> a -> m a

union :: (MonadRef m, Relational m a) => RelM m a -> RelM m a -> m ()
union m n = do
  (mpos, msize, ma, mroot) <- findEx m
  (npos, nsize, na, nroot) <- findEx n
  unless (nroot == mroot) $if msize <= nsize
    then do
      writeRef mroot $ Child (nsize + mpos) nroot
      na' <- disjointUnion nsize na msize ma
      writeRef nroot $ Root (msize + nsize) na'
    else do
      writeRef nroot $ Child (msize + npos) mroot
      ma' <- disjointUnion msize ma nsize na
      writeRef mroot $ Root (nsize + msize) ma'

-- TODO: build rational octagons out of these parts?
