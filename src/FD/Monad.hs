{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language OverloadedLists #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module FD.Monad where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Default
import Data.Word
import Logic.Class
import Logic.Reflection as Reflection
import Par.Cont as Cont
import Signal

type FD' s = ReaderT (SignalEnv (FD s)) (Reflection.LogicT (ST s))

type M = FD

newtype FD s a = FD { runFD :: Cont.Par (FD' s) a } deriving
  ( Functor, Applicative, Alternative
  , Monad, MonadPlus
  , MonadReader (SignalEnv (FD s))
  , PrimMonad
  )

instance MonadLogic (FD s) where
  msplit (FD m) = FD $ fmap FD <$> msplit m
  interleave = (<|>)

unFD :: FD s a -> LogicT (ST s) a
unFD m = do
  se <- newSignalEnv
  runReaderT (evalStateT (statePar (runFD m)) def) se

run1 :: (forall s. FD s a) -> a
run1 m = runST $ observeT $ unFD m

runN :: Int -> (forall s. FD s a) -> [a]
runN n m = runST $ observeManyT n $ unFD m

run :: (forall s. FD s a) -> [a]
run m = runST $ observeAllT $ unFD m

withMultiplicity :: FD s a -> FD s (a, Maybe Word64)
withMultiplicity m = (,) <$> m <*> currentMultiplicity
