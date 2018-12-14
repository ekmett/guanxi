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
module Ref.Disjoint where

import Control.Applicative (liftA2)
import Ref

data Content m
  = Root {-# unpack #-} !Int
  | Child (Disjoint m)

newtype Disjoint m = Disjoint { getDisjoint :: Ref m (Content m) }
  deriving Eq

instance Reference m (Content m) (Disjoint m) where
  reference = getDisjoint

-- return rank as well as result
findEx :: MonadRef m => Disjoint m -> m (Int, Disjoint m)
findEx d = readRef d >>= \case
  Root i -> pure (i, d)
  Child s -> do
    x <- findEx s
    x <$ writeRef d (Child $ snd x)

find :: MonadRef m => Disjoint m -> m (Disjoint m)
find d = readRef d >>= \case
  Root _ -> pure d
  Child s -> do
    x <- find s
    x <$ writeRef d (Child x)

union :: MonadRef m => Disjoint m -> Disjoint m -> m ()
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
eq :: MonadRef m => Disjoint m -> Disjoint m -> m Bool
eq m n = liftA2 (==) (find m) (find n)
