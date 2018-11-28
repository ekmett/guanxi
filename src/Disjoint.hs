{-# language TemplateHaskell #-}
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
-- disjoint set forests
module Disjoint 
  ( Disjoint
  , newDisjoint
  , union
  , find
  , size
  , eq
  ) where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State.Class
import Key
import Ref

data D u = D
  { _dsize :: {-# unpack #-} !Int
  , _parent :: {-# unpack #-} !(Disjoint u)
  }

newtype Disjoint u = Disjoint { runDisjoint :: Ref u (D u) }
  deriving (Eq)

makeLenses ''D

instance Reference (Disjoint u) u (D u) where
  reference = runDisjoint

newDisjoint ::
  ( MonadKey m
  , MonadState s m
  , HasRefEnv s (KeyState m)
  ) => m (Disjoint (KeyState m))
newDisjoint = Disjoint <$> newSelfRef (D 1 . Disjoint)

-- | find with path splitting
find :: (MonadState s m, HasRefEnv s u) => Disjoint u -> m (Disjoint u)
find x = do
  n <- use (ref x.parent)
  if n == x then pure x else do
    ref x.parent <~ use (ref n.parent)
    find n

-- | find with path splitting, returning rank as well as answer
findD :: (MonadState s m, HasRefEnv s u) => Disjoint u -> m (D u)
findD x = do
  f@(D _ n) <- use (ref x)
  if n == x then pure f else do
    ref x.parent <~ use (ref n.parent)
    findD n

-- | union by size
union :: (MonadState s m, HasRefEnv s u) => Disjoint u -> Disjoint u -> m ()
union x y = do
  D xsize xroot <- findD x 
  D ysize yroot <- findD y
  unless (xroot == yroot) $ if xsize <= ysize
    then do
      ref xroot.parent .= yroot
      ref yroot.dsize += xsize 
    else do
      ref yroot.parent .= xroot
      ref xroot.dsize += ysize 

eq :: (MonadState s m, HasRefEnv s u) => Disjoint u -> Disjoint u -> m Bool
eq x y = (==) <$> find x <*> find y

size :: (MonadState s m, HasRefEnv s u) => Disjoint u -> m Int
size = fmap _dsize . findD
