{-# language DeriveTraversable #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}

-- | Unsafe environment operations
module Skew
  ( Skew(..)
  , var, var'
  , lookup
  , Nil(..)
  , Cons(..)
  ) where

import Control.Lens (fusing, Lens')
import Data.Bits
import Data.Functor
import Prelude hiding (lookup)
import Unaligned

data Tree a
  = Bin a !(Tree a) !(Tree a)
  | Tip a
  deriving (Show, Foldable, Functor, Traversable)

data Spine a
  = Nil
  | Cons !Int !(Tree a) !(Spine a)
  deriving (Show, Foldable, Functor, Traversable)

instance Nil Spine where
  nil = Nil
  {-# inline conlike nil #-}

instance Cons Spine where
  cons a (Cons i x (Cons j y zs)) | i == j = Cons (i+j+1) (Bin a x y) zs
  cons a xs = Cons 1 (Tip a) xs
  {-# inline conlike cons #-}

data Skew a = Skew {-# unpack #-} !Int !(Spine a)
  deriving Show

instance Nil Skew where
  nil = Skew 0 Nil
  {-# inline conlike nil #-}

instance Cons Skew where
  cons a (Skew i xs) = Skew (i+1) (cons a xs)
  {-# inline conlike cons #-}

lookup :: Int -> Skew a -> a
lookup i (Skew j xs)
  | i > j     = error "variable impossibly new"
  | otherwise = lookupSpine (j - i - 1) xs

lookupSpine :: Int -> Spine a -> a 
lookupSpine _ Nil = error "variable impossibly old"
lookupSpine i (Cons j t xs)
  | i < j     = lookupTree i j t
  | otherwise = lookupSpine (i - j) xs

lookupTree :: Int -> Int -> Tree a -> a
lookupTree _ _ (Tip a) = a
lookupTree i j (Bin a l r)
  | i == 0 = a
  | i <= j' = lookupTree i j' l
  | otherwise = lookupTree (i-j'-1) j' r
  where j' = unsafeShiftR j 1

-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

var :: Int -> Lens' (Skew a) a
var = fusing . var'

var' :: Int -> Lens' (Skew a) a
var' i f (Skew j xs)
  | i > j     = error "variable impossibly new"
  | otherwise = Skew j <$> varSpine (j - i - 1) f xs

varSpine :: Int -> Lens' (Spine a) a
varSpine _ _ Nil = error "variable impossibly old"
varSpine i f (Cons j t xs)
  | i < j     = (\t' -> Cons j t' xs) <$> varTree i j f t
  | otherwise = Cons j t <$> varSpine (i - j) f xs

varTree :: Int -> Int -> Lens' (Tree a) a
varTree _ _ f (Tip a) = Tip <$> f a
varTree i j f (Bin a l r)
  | i == 0    = (\a' -> Bin a' l r) <$> f a
  | i <= j'   = (\l' -> Bin a l' r) <$> varTree i j' f l
  | otherwise = (\r' -> Bin a l r') <$> varTree (i-j'-1) j' f r
  where j' = unsafeShiftR j 1
