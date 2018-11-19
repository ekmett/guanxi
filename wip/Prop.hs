{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Main where

-- this is a minimalist demo finite domain solver
-- written by hand, it needs Env and Logic.Reflection
-- from guanxi to run, though

import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict as Strict
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.Type.Coercion
import qualified Data.IntSet as IntSet
import Data.Type.Equality
import qualified Data.Set as Set

import Key
import Logic.Class
import Logic.Reflection

type Id = Int

data Var s a = Var !Id !(Key s a)
  deriving Eq

instance Show (Var s a) where
  showsPrec d (Var i _) = showsPrec d i

instance TestEquality (Var s) where
  testEquality (Var i k) (Var j l) = do
    guard (i == j)
    testEquality k l

instance TestCoercion (Var s) where
  testCoercion (Var i k) (Var j l) = do
    guard (i == j)
    testCoercion k l

newtype VarSet s = VarSet { getVarSet :: IntSet } deriving (Eq,Ord,Show,Semigroup,Monoid)

singleton :: Var s a -> VarSet s
singleton (Var i _) = VarSet (IntSet.singleton i)

newtype Constraint s a = Constraint { runConstraint :: a -> FD s (VarSet s) }

instance Semigroup (Constraint s a) where
  f <> g = Constraint $ \s -> (<>) <$> runConstraint f s <*> runConstraint g s

instance Monoid (Constraint s a) where
  mempty = Constraint $ \_ -> return mempty

instance Show (Constraint s a) where
  showsPrec _ _ = showString "Constraint{}"

type Strategy s a = Var s a -> a -> FD s ()

data Val s where
  Val :: Key s a -> a -> Constraint s a -> Strategy s a -> Val s

instance Show (Val s) where
  showsPrec _ _ = showString "Val{}"

data Env s = Env
  { _fresh   :: !Id
  , _content :: !(HashMap Id (Val s))
  } deriving Show

type FD s = Strict.StateT (Env s) (LogicT (ST s))

makeLenses ''Env

simple :: Var s b -> (a -> b -> Maybe b) -> Constraint s a
simple y f = Constraint $ \xs -> guts y . _1 %%= \ ys -> case f xs ys of
  Nothing  -> (mempty, ys)
  Just ys' -> (singleton y, ys')

unlockVal :: Key s a -> Traversal' (Val s) (a, Constraint s a, Strategy s a)
unlockVal k f v@(Val l s cs ss) = case testEquality k l of
  Just Refl -> f (s, cs, ss) <&> \(s', cs', ss') -> Val l s' cs' ss'
  Nothing -> pure v

guts :: Var s a -> Lens' (Env s) (a, Constraint s a, Strategy s a)
guts (Var x k) = singular (content.ix x.unlockVal k)

newVar :: Ord a => a -> (Var s a -> a -> FD s ()) -> FD s (Var s a)
newVar s strat = do
  k <- newKey
  n <- fresh <<+= 1
  content.at n ?= Val k s mempty strat
  pure (Var n k)

fire :: VarSet s -> FD s ()
fire vs = case IntSet.minView (getVarSet vs) of
  Nothing -> pure ()
  Just (v, vs') -> do
    Val _ xs c _ <- use $ singular (content.ix v)
    us <- runConstraint c xs
    fire $ VarSet vs' <> us

mutual
  :: (a -> b -> Maybe b)
  -> (b -> a -> Maybe a)
  -> Var s a -> Var s b -> FD s ()
mutual f g x y = do
  guts x._2 <>= simple y f
  guts y._2 <>= simple x g
  fire $ singleton x <> singleton y

lt :: Ord a => Var s (Set a) -> Var s (Set a) -> FD s ()
lt x y = do
  guard (x /= y)
  let f xs ys = case Set.minView xs of
        Nothing -> undefined
        Just (min_x,_) -> case Set.splitMember min_x ys of
          (a, b, ys') -> ys' <$ guard (not (null a) || b)
      g ys xs = case Set.maxView ys of
        Nothing -> undefined
        Just (max_y,_) -> case Set.splitMember max_y xs of
          (xs',b,a) -> xs' <$ guard (not (null a) || b)
  mutual f g x y

ne :: Ord a => Var s (Set a) -> Var s (Set a) -> FD s ()
ne x y = do
  guard (x /= y)
  let f xs ys
       | length xs == 1
       , Just (v,_) <- Set.minView xs
       , (b, ys') <- ys & contains v <<.~ False
       = ys'<$ guard b
        | otherwise = Nothing
  mutual f f x y

eq :: Ord a => Var s (Set a) -> Var s (Set a) -> FD s ()
eq x y = when (x /= y) $ do
  let f xs ys = Set.intersection xs ys
             <$ guard (not (null (Set.difference ys xs)))
  mutual f f x y

is :: Ord a => Var s (Set a) -> a -> FD s ()
is x v = join $ guts x._1 %%= \xs -> if
  | Set.member v xs -> (when (length xs /= 1) $ fire (singleton x), Set.singleton v)
  | otherwise -> (empty, xs)

-- run this one term to ground so we can look at it
val :: Var s (Set a) -> FD s a
val x = use (guts x . _1) >>= foldr (interleave . is_) empty where
  is_ v = (<$) v $ join $ guts x._1 %%= \xs -> (when (length xs /= 1) $ fire (singleton x), Set.singleton v)

ground :: FD s ()
ground = do
  m <- use content
  ifor_ m $ \i (Val k a _ strat) -> strat (Var i k) a -- void $ val $ Var x l

eval :: FD s a -> LogicT (ST s) a
eval m = fst <$> runStateT (m <* ground) (Env 0 mempty)

run1 :: (forall s. FD s a) -> a
run1 m = runST $ observeT $ eval m

runN :: Int -> (forall s. FD s a) -> [a]
runN n m = runST $ observeManyT n $ eval m

run :: (forall s. FD s a) -> [a]
run m = runST $ observeAllT $ eval m

newSetVar :: Ord a => Set a -> FD s (Var s (Set a))
newSetVar s = newVar s strat where
  strat x = foldr (interleave . is_) empty where
    is_ v = join $ guts x._1 %%= \xs -> (when (length xs /= 1) $ fire (singleton x), Set.singleton v)

example :: FD s (Int, Int, Int, Bool)
example = do
  x <- newSetVar [1..5]
  y <- newSetVar [1..5]
  z <- newSetVar [1..4]
  w <- newSetVar [False .. True]
  lt x y
  ne y z
  eq x z
  is w False
  (,,,) <$> val x <*> val y <*> val z <*> val w

main :: IO ()
main = print $ run example
