{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# language OverloadedLists #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies #-}
{-# language StandaloneDeriving #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module FD.Monad where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.ST
import FD.Var
import Logic.Class
import Logic.Reflection as Reflection
import Par.Cont as Cont
import Ref.Key
import Ref.Signal

type FD' s = StateT (SignalEnv (FD s)) (Reflection.LogicT (ST s))

newtype FD s a = FD { runFD :: Cont.Par (FD' s) a } deriving
  ( Functor, Applicative, Alternative
  , Monad, MonadPlus
  , MonadState (SignalEnv (FD s))
  , PrimMonad
  , MonadKey
  )

instance MonadLogic (FD s) where
  msplit (FD m) = FD $ fmap FD <$> msplit m

eval :: FD s a -> LogicT (ST s) a
eval m = evalStateT (evalStateT (statePar (runFD m)) defaultParEnv) defaultSignalEnv

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
