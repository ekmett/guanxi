{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module FD.Var where

import Control.Applicative as A
import Control.Lens
import Control.Monad (join, when, guard)
import Control.Monad.State.Class
import Data.Set as Set
import Data.Type.Coercion
import Logic.Class
import Ref.Base
import Ref.Key
import Ref.Signal

-- finite domains represented as intersection sets with
-- propagators to establish generalized arc consistency,
-- followed by a final concretization pass
data FDVar m a = FDVar (Ref (KeyState m) (Set a)) (Signal m)

instance Eq (FDVar m a) where
  FDVar _ s == FDVar _ t = s == t

instance TestCoercion (FDVar m) where
  testCoercion (FDVar r i) (FDVar s j) = case testCoercion r s of
     Just Coercion | i == j -> Just Coercion
     _ -> Nothing

instance HasSignals m (FDVar m a) where
  signals (FDVar _ v) = signals v

instance (u ~ KeyState m) => Reference (FDVar m a) u (Set a) where
  reference (FDVar r _) = r

newFDVar
  :: ( MonadLogic m
     , MonadKey m
     , MonadState s m
     , HasSignalEnv s m
     , Ord a
     ) => Set a -> m (FDVar m a)
newFDVar dom = do
  rdom <- newRef dom
  let is_ v a = join $ ref rdom %%= \ s -> (,Set.singleton a) $ when (Set.size s /= 1) $ fire v
  fmap (FDVar rdom) $ newSignal $ \v -> readRef rdom >>= Set.foldr (interleave . is_ v) A.empty

val :: (MonadLogic m, MonadState s m, HasSignalEnv s m) => FDVar m a -> m a
val r = do
  let is_ a = join $ ref r %%= \ s -> (,Set.singleton a) $ a <$ when (Set.size s /= 1) (fire r)
  readRef r >>= Set.foldr (interleave . is_) A.empty

-- unsafe
shrink :: (MonadState s m, HasSignalEnv s m, Alternative m) => FDVar m a -> (Set a -> Set a) -> m ()
shrink r f = join $ ref r %%= \d@(f -> d') -> (,d') $ do
  guard $ not $ Set.null d' -- ensure there is an answer
  when (Set.size d' /= Set.size d) $ fire r

is :: (MonadState s m, HasSignalEnv s m, Alternative m, Ord a) => FDVar m a -> a -> m ()
is v a = shrink v $ \d -> if Set.member a d then Set.singleton a else Set.empty

isn't :: (MonadState s m, HasSignalEnv s m, Alternative m, Ord a) => FDVar m a -> a -> m ()
isn't v a = shrink v (Set.delete a)

lt :: (MonadState s m, HasSignalEnv s m, Alternative m, Ord a) => FDVar m a -> FDVar m a -> m ()
lt l r = do
  guard (l /= r)
  propagate l r $ readRef l >>= \ xs -> case Set.minView xs of
    Nothing -> A.empty
    Just (min_x,_) -> shrink r $ \ ys -> Set.splitMember min_x ys ^. _3
  propagate r l $ readRef r >>= \ ys -> case Set.maxView ys of
    Nothing -> A.empty
    Just (max_y,_) -> shrink l $ \ xs -> Set.splitMember max_y xs ^. _1
