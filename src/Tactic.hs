{-# language RankNTypes #-}
{-# language DeriveTraversable #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A toy tactic language
--
-- e.g. @Tactic ([Formula],Formula) FD@

module Tactic where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Control.Applicative
import Control.Monad (ap, MonadPlus(..))

data T s a = Proof a | Goal s | Failed
  deriving (Functor,Foldable,Traversable)
 
instance Bifunctor T where bimap = bimapDefault
instance Bifoldable T where bifoldMap = bifoldMapDefault
instance Bitraversable T where
  bitraverse _ g (Proof b) = Proof <$> g b
  bitraverse f _ (Goal a) = Goal <$> f a
  bitraverse _ _ Failed = pure Failed

-- uncps
lower :: Applicative m => Tactic g m a -> g -> m (T g a)
lower m = runTactic m (pure . Proof) (pure Failed) (pure . Goal)


-- | Based on 
-- <https://people.eecs.berkeley.edu/~necula/autded/lecture15-tactics.pdf>
newtype Tactic g m a = Tactic
  { runTactic :: forall r.
    (a -> m r) -> m r -> (g -> m r) -> g -> m r
  } deriving Functor
 
instance Applicative (Tactic g m) where
  pure a = Tactic $ \kp _ _ _ -> kp a
  (<*>) = ap

instance Monad (Tactic g m) where
  m >>= f = Tactic $ \kp kf ks g ->
    runTactic m (\a -> runTactic (f a) kp kf ks g) kf ks g

fby :: Tactic g m a -> Tactic g m a -> Tactic g m a
fby m n = Tactic $ \kp kf kc ->
  runTactic m kp kf (runTactic n kp kf kc) 

instance Alternative (Tactic g m) where
  m <|> n = Tactic $ \kp kf kc g ->
    runTactic m kp (runTactic n kp kf kc g) kc g
  empty = Tactic $ \_ kf _ _ -> kf

instance MonadPlus (Tactic g m) where
  mplus = (<|>)
  mzero = empty

repeatedly :: Tactic g m a -> Tactic g m a
repeatedly t = t `fby` repeatedly t
