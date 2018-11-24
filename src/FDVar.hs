{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}

module FDVar where

import Cell
import Control.Applicative as A
import Control.Lens 
import Control.Monad (join, when, guard)
import Control.Monad.State.Class
import Data.Set as Set
import Key
import Logic.Class
import Ref

-- intersection sets with a final concretization pass
data FDVar m a = FDVar
  { current :: Ref (KeyState m) (Set a)
  , getFDVar :: Var m
  }

instance HasCellIds (FDVar m a) where
  cellIds (FDVar _ v) = cellIds v

newFDVar :: (MonadLogic m, MonadKey m, MonadState s m, HasCellEnv s m, Ord a) => Set a -> m (FDVar m a)
newFDVar dom = do
  rdom <- newRef dom
  let is_ v a = join $ ref rdom %%= \ s -> (,Set.singleton a) $ when (Set.size s /= 1) $ fireVars v
  fmap (FDVar rdom) $ newVar $ \v -> readRef rdom >>= Set.foldr (interleave . is_ v) A.empty 

shrink :: (MonadState s m, HasCellEnv s m, Alternative m) => FDVar m a -> (Set a -> Set a) -> m ()
shrink (FDVar r v) f = join $ ref r %%= \d@(f -> d') -> (,d') $ do
  guard (not $ Set.null d') -- ensure there is an answer
  when (Set.size d' /= Set.size d) $ fireVars v

is :: (MonadState s m, HasCellEnv s m, Alternative m, Ord a) => FDVar m a -> a -> m ()
is v a = shrink v $ \d -> if Set.member a d then Set.singleton a else Set.empty

isn't :: (MonadState s m, HasCellEnv s m, Alternative m, Ord a) => FDVar m a -> a -> m ()
isn't v a = shrink v (Set.delete a)

-- construct a pair of propagators between these
lt :: (MonadState s m, HasCellEnv s m, Alternative m, Ord a) => FDVar m a -> FDVar m a -> m ()
lt fl@(FDVar dl l) fr@(FDVar dr r) = do
  guard (l /= r) -- we're never less than ourself
  newPropagator_ fl fr $ readRef dl >>= \ xs -> case Set.minView xs of
    Nothing -> A.empty
    Just (min_x,_) -> shrink fr $ \ ys -> Set.splitMember min_x ys ^. _3
  newPropagator_ fr fl $ readRef dr >>= \ ys -> case Set.maxView ys of
    Nothing -> A.empty
    Just (max_y,_) -> shrink fl $ \ xs -> Set.splitMember max_y xs ^. _1
