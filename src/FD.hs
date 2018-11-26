{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# language OverloadedLists #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module FD where

import Cell
import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.ST
import FDVar
import Key
import Logic.Class
import Logic.Reflection

newtype FD s a = FD { runFD :: StateT (CellEnv (FD s)) (LogicT (ST s)) a } deriving
  ( Functor, Applicative, Alternative
  , Monad, MonadPlus
  , MonadState (CellEnv (FD s))
  , PrimMonad
  , MonadKey
  )

instance MonadLogic (FD s) where
  msplit (FD m) = FD (fmap FD <$> msplit m)
  interleave (FD m) (FD n) = FD (interleave m n)
  FD m >>- f = FD (m >>- (runFD . f))
  ifte (FD m) f (FD e) = FD (ifte m (runFD . f) e)
  once (FD m) = FD (once m)
 
eval :: FD s a -> LogicT (ST s) a
eval m = evalStateT (runFD m) defaultCellEnv
 
run1 :: (forall s. FD s a) -> a
run1 m = runST $ observeT $ eval m

runN :: Int -> (forall s. FD s a) -> [a]
runN n m = runST $ observeManyT n $ eval m

run :: (forall s. FD s a) -> [a]
run m = runST $ observeAllT $ eval m

-- |
-- >>> run example
-- [(1,2),(1,3),(2,3)]
example :: FD s (Integer, Integer)
example = do
  x <- newFDVar [1..3]
  y <- newFDVar [1..3]
  lt x y
  (,) <$> val x <*> val y
