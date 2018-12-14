{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Rem's interleaved algorithm for union-find.
module Ref.Disjoint where

{-
import Ref.Base

import Control.Lens
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.Primitive
import Control.Monad.State.Class
import Data.Function


-- | (flipped) Rem's invariant: all pointers go to lower ids
-- This causes us to favor old IDs, that way when we allocate
-- fresh IDs we aren't constantly 'pulling' the pool of
-- equivalences into the new IDs.

-- this now requires us to be able to supply ids as rem ids aren't ordered

newtype Rem m = Rem { runRem :: Ref m (Rem m) } deriving Eq

instance Reference m (Rem m) (Rem m) where
  reference = runRem

newRem :: PrimState m => m (Rem (PrimState m))
newRem = Rem <$> newSelfRef Rem

eq :: MonadRef m => Rem m -> Rem m -> m Bool
eq = liftA2 (==) `on` find -- this could be smarter

-- | find with path splitting
find :: MonadRef m => Rem m -> m (Rem m)
find x = do
  n <- readRef x
  if n == x then pure x else do -- path splitting
    readRef n >>= writeRef x
    find n

-- | RemSP(x,y)
union :: MonadRef m => Rem m -> Rem m -> m ()
union x y = do
  px <- readRef x
  py <- readRef y
  case compare px py of
    EQ -> pure ()
    LT -> do
      ref y .= px
      unless (y == py) $ union x py
    GT -> do
      ref x .= py
      unless (x == px) $ union px y
-}
