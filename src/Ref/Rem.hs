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
module Ref.Rem
  ( Rem
  , newRem
  , union
  , find
  , eq
  ) where

import Control.Lens
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.State.Class
import Data.Function
import Ref.Base
import Ref.Key

-- | (flipped) Rem's invariant: all pointers go to lower ids
-- This causes us to favor old IDs, that way when we allocate
-- fresh IDs we aren't constantly 'pulling' the pool of
-- equivalences into the new IDs.
newtype Rem u = Rem { runRem :: Ref u (Rem u) } deriving (Eq,Ord)

instance Reference (Rem u) u (Rem u) where
  reference = runRem

newRem :: (MonadKey m , MonadState s m , HasRefEnv s (KeyState m)) => m (Rem (KeyState m))
newRem = Rem <$> newSelfRef Rem

eq :: (MonadState s m, HasRefEnv s u) => Rem u -> Rem u -> m Bool
eq = liftA2 (==) `on` find -- this could be smarter

-- | find with path splitting
find :: (MonadState s m, HasRefEnv s u) => Rem u -> m (Rem u)
find x = do
  n <- use (ref x)
  if n == x then pure x else do -- path splitting
    ref x <~ use (ref n)
    find n

-- | RemSP(x,y)
union :: (MonadState s m, HasRefEnv s u) => Rem u -> Rem u -> m ()
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
