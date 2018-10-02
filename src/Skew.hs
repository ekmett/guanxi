{-# language DeriveTraversable #-}
{-# language RankNTypes #-}

module Skew
  ( Env(..)
  , var
  , lookup
  , Nil(..)
  , Cons(..)
  , Cons_(..)
  , allocate
  ) where

import Data.Bits
import Data.Semigroup
import Unaligned
import Prelude hiding (lookup)

data Tree a
  = Bin a !(Tree a) !(Tree a)
  | Bin_ !(Tree a) !(Tree a)
  | Tip
  deriving (Show, Foldable, Functor, Traversable)

data Spine a = Cons !Int !(Tree a) !(Spine a) | Nil
  deriving (Show, Foldable, Functor, Traversable)

bin :: Maybe a -> Tree a -> Tree a -> Tree a
bin Nothing Tip Tip = Tip
bin Nothing l r = Bin_ l r
bin (Just a) l r = Bin a l r

bin_ :: Tree a -> Tree a -> Tree a
bin_ Tip Tip = Tip
bin_ l r = Bin_ l r

instance Nil Spine where
  nil = Nil

instance Cons Spine where
  cons a (Cons i x (Cons j y zs)) | i == j = Cons (i+j+1) (Bin a x y) zs
  cons a xs = Cons 1 (Bin a Tip Tip) xs

class Cons_ t where
  cons_ :: t a -> t a

instance Cons_ Spine where
  cons_ (Cons i x (Cons j y zs)) | i == j = Cons (i+j+1) (bin_ x y) zs
  cons_ xs = Cons 1 Tip xs

data Env a = Env {-# unpack #-} !Int {-# unpack #-} !Int !(Spine a)
  deriving Show

instance Nil Env where
  nil = Env 0 0 nil

instance Cons Env where
  cons a (Env i j xs) = Env (i+1) (j+1) (cons a xs)

instance Cons_ Env where
  cons_ (Env i j xs) = Env (i+1) (j+1) (cons_ xs)

allocate :: Int -> Env a -> Env a
allocate i (Env j k xs) = Env (j+i) (k+i) $ appEndo (stimesMonoid i (Endo cons_)) xs

lookup :: Int -> Env a -> Maybe a
lookup i (Env j k xs)
  | i > j     = error "variable impossibly new"
  | i > k     = Nothing
  | otherwise = lookupSpine (k - i - 1) xs

lookupSpine :: Int -> Spine a -> Maybe a 
lookupSpine _ Nil = error "variable impossibly old"
lookupSpine i (Cons j t xs)
  | i < j     = lookupTree i j t
  | otherwise = lookupSpine (i - j) xs

lookupTree :: Int -> Int -> Tree a -> Maybe a
lookupTree _ _ Tip = Nothing
lookupTree i j (Bin a l r)
  | i == 0 = Just a
  | i <= j' = lookupTree i j' l
  | otherwise = lookupTree (i-j'-1) j' r
  where j' = unsafeShiftR j 1
lookupTree i j (Bin_ l r)
  | i == 0 = Nothing
  | i <= j' = lookupTree i j' l
  | otherwise = lookupTree (i-j'-1) j' r
  where j' = unsafeShiftR j 1

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- TODO: actualVar i = fusing (var i)

var :: Int -> Lens' (Env a) (Maybe a)
var i f (Env j k xs)
  | i > j     = error "variable impossibly new"
  -- | i > k     = <$> f Nothing now we need to extend to the left
  | otherwise = Env j k <$> varSpine (k - i - 1) f xs
{-# inline var #-}

varSpine :: Int -> Lens' (Spine a) (Maybe a)
varSpine _ _ Nil = error "variable impossibly old"
varSpine i f (Cons j t xs)
  | i < j     = (\t' -> Cons j t' xs) <$> varTree i j f t
  | otherwise = Cons j t <$> varSpine (i - j) f xs

tweak :: Int -> Int -> Maybe a -> Tree a
tweak _ _ Nothing = Tip
tweak i0 j0 (Just a0) = go i0 j0 a0 where
  go :: Int -> Int -> a -> Tree a
  go i j a 
    | i == 0 = Bin a Tip Tip
    | i <= j' = Bin_ (go i j' a) Tip
    | otherwise = Bin_ Tip $ go (i-j'-1) j' a
    where j' = unsafeShiftR j 1
{-# inline tweak #-}

varTree :: Int -> Int -> Lens' (Tree a) (Maybe a)
varTree i j f Tip = tweak i j <$> f Nothing
varTree i j f (Bin a l r)
  | i == 0    = (\ma -> bin ma l r) <$> f (Just a)
  | i <= j'   = (\l' -> Bin a l' r) <$> varTree i j' f l
  | otherwise = (\r' -> Bin a l r') <$> varTree (i-j'-1) j' f r
  where j' = unsafeShiftR j 1
varTree i j f (Bin_ l r)
  | i == 0    = (\ma -> bin ma l r) <$> f Nothing
  | i <= j'   = (\l' -> bin_ l' r) <$> varTree i j' f l
  | otherwise = (\r' -> bin_ l r') <$> varTree (i-j'-1) j' f r
  where j' = unsafeShiftR j 1
