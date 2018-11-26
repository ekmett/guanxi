{-# language DeriveTraversable #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language ExplicitNamespaces #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Skew-binary random-access lists with cheaper missing entries.
-- This is constructed in a similar fashion to the @desbral@
-- implementation from 
-- <https://users.soe.ucsc.edu/~lkuper/papers/walk.pdf Efficient representation of triangular substititons: A comparison in miniKanren>
-- by Bender, Kuper, Byrd and Friedman
--
-- Note: @cons@ and @nil@ are supplied by @Unaligned@
module Internal.Skew
  ( Skew
  , var, var'
  , lookup
  , Cons_(..)
  , allocate
  , empty
  , size
  ) where

import Control.Lens
  ( fusing
  , Lens'
  , FunctorWithIndex(..)
  , FoldableWithIndex(..)
  , TraversableWithIndex(..)
  )
import Data.Bits
import Data.Functor
import Prelude hiding (lookup)
import Unaligned

shr :: Int -> Int
shr j = unsafeShiftR j 1
{-# inline shr #-}

smear :: Int -> Int
smear i0 = i5 .|. unsafeShiftR i5 32 where
      i5 = i4 .|. unsafeShiftR i4 16
      i4 = i3 .|. unsafeShiftR i3 8
      i3 = i2 .|. unsafeShiftR i2 4
      i2 = i1 .|. unsafeShiftR i1 2
      i1 = i0 .|. unsafeShiftR i0 1

data Tree a
  = Bin a !(Tree a) !(Tree a)
  | Bin_ !(Tree a) !(Tree a)
  | Tip
  deriving (Show, Foldable, Functor, Traversable)

data Spine a = C !Int !(Tree a) !(Spine a) | N
  deriving (Show, Foldable, Functor, Traversable)

instance FunctorWithIndex Int Spine where imapped = itraversed
instance FoldableWithIndex Int Spine where ifolded = itraversed

instance TraversableWithIndex Int Spine where
  itraverse f = go 0 where
    go !_ N = pure N
    go i (C j t s) = C j <$> goTree i j t <*> go (i+j) s
    goTree _ _ Tip = pure Tip
    goTree i (shr -> j) (Bin a l r) = Bin <$> f i a <*> goTree (i-1) j l <*> goTree (i-j-1) j r
    goTree i (shr -> j) (Bin_ l r) = Bin_ <$> goTree (i-1) j l <*> goTree (i-j-1) j r
  {-# inline itraverse #-}

bin :: Maybe a -> Tree a -> Tree a -> Tree a
bin Nothing Tip Tip = Tip
bin Nothing l r = Bin_ l r
bin (Just a) l r = Bin a l r
{-# inline conlike bin #-}

bin_ :: Tree a -> Tree a -> Tree a
bin_ Tip Tip = Tip
bin_ l r = Bin_ l r
{-# inline conlike bin_ #-}

padSpine :: Int -> Spine a -> Spine a
padSpine 0 xs = xs
padSpine i N = padSpine' i (smear i) N
padSpine i xs@(C j x N)
  | i >= j+1  = padSpine (i-j+1) $ C (j+j+1) (bin_ Tip x) N
  | otherwise = padSpine' i (unsafeShiftR j 1) xs
padSpine i xs@(C j x ys@(C k y zs))
  -- climb up and inflate heads as needed
  | j == k    = padSpine (i-1)   $ C (j+k+1) (bin_ x y) zs
  | i >= j+1  = padSpine (i-j+1) $ C (j+j+1) (bin_ Tip x) ys
  | otherwise = padSpine' i (unsafeShiftR j 1) xs

padSpine' :: Int -> Int -> Spine a -> Spine a
padSpine' 0 _ ws = ws
padSpine' i j ws
  | i >= j = padSpine' (i-j) j $ C j Tip ws
  | otherwise = padSpine' i (unsafeShiftR j 1) ws

instance Nil Spine where
  nil = N
  {-# inline conlike nil #-}

instance Cons Spine where
  cons a (C i x (C j y zs)) | i == j = C (i+j+1) (Bin a x y) zs
  cons a xs = C 1 (Bin a Tip Tip) xs
  {-# inline conlike cons #-}

class Cons_ t where
  cons_ :: t a -> t a

instance Cons_ Spine where
  cons_ (C i x (C j y zs)) | i == j = C (i+j+1) (bin_ x y) zs
  cons_ xs = C 1 Tip xs
  {-# inline conlike cons_ #-}

data Skew a = Skew {-# unpack #-} !Int {-# unpack #-} !Int !(Spine a)
  deriving (Functor, Foldable, Traversable, Show)

-- | The empty skew-binary random-access list
-- >>> size empty
-- 0
empty :: Skew a
empty = Skew 0 0 N

-- | Number of allocated nodes
size :: Skew a -> Int
size (Skew i _ _) = i

instance Nil Skew where
  nil = Skew 0 0 nil
  {-# inline conlike nil #-}

instance Cons Skew where
  cons a (Skew i j xs) = Skew (i+1) (i+1) (cons a (padSpine (i-j) xs))
  {-# inline conlike cons #-}

instance Cons_ Skew where
  cons_ (Skew i j xs) = Skew (i+1) j xs
  {-# inline conlike cons_ #-}

instance FoldableWithIndex Int Skew where ifolded = itraversed
instance FunctorWithIndex Int Skew where imapped = itraversed
instance TraversableWithIndex Int Skew where
  itraverse f (Skew j k s) = Skew j k <$> itraverse (\i -> f (k-1-i)) s

-- | @allocate i s@ returns an integer @j@, and an updated skew-binary random-access
-- list such that slots @[j..j+i-1]@ are usable and are initialized to Nothing
--
-- >>> size $ snd $ allocate 10 empty
-- 10
--
-- >>> lookup 0 $ snd $ allocate 1 empty
-- Nothing
allocate :: Int -> Skew a -> (Int, Skew a)
allocate i (Skew j k xs) = (j, Skew (j+i) k xs)

-- | @lookup@ indexes into the skew-binary random-access list from the tail end
-- so that names are stable under @cons@ and @allocate@
--
-- >>> lookup 0 $ cons "world" empty
-- Just "world"
--
-- >>> lookup 0 $ cons "hello" $ cons "world" empty
-- Just "world"
--
-- >>> lookup 1 $ cons "hello" $ cons "world" empty
-- Just "hello"
lookup :: Int -> Skew a -> Maybe a
lookup i (Skew j k xs)
  | i > j     = error "variable impossibly new"
  | i >= k    = Nothing
  | otherwise = lookupSpine (k - i - 1) xs

lookupSpine :: Int -> Spine a -> Maybe a
lookupSpine _ N = error "variable impossibly old"
lookupSpine i (C j t xs)
  | i < j     = lookupTree i j t
  | otherwise = lookupSpine (i - j) xs

lookupTree :: Int -> Int -> Tree a -> Maybe a
lookupTree _ _ Tip = Nothing
lookupTree i (shr -> j) (Bin a l r)
  | i == 0 = Just a
  | i <= j = lookupTree (i-1) j l
  | otherwise = lookupTree (i-j-1) j r
lookupTree i (shr -> j) (Bin_ l r)
  | i == 0 = Nothing
  | i <= j = lookupTree (i-1) j l
  | otherwise = lookupTree (i-j-1) j r

-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

var :: Int -> Lens' (Skew a) (Maybe a)
var = fusing . var'

var' :: Int -> Lens' (Skew a) (Maybe a)
var' i f (Skew j k xs)
  | i > j     = error "variable impossibly new"
  | i > k     = f Nothing <&> \case
    Nothing -> Skew j k xs
    Just a  -> Skew j (i+1) $ cons a $ padSpine (i - k - 1) xs
  | otherwise = Skew j k <$> varSpine (k - i - 1) f xs

varSpine :: Int -> Lens' (Spine a) (Maybe a)
varSpine _ _ N = error "variable impossibly old"
varSpine i f (C j t xs)
  | i < j     = (\t' -> C j t' xs) <$> varTree i j f t
  | otherwise = C j t <$> varSpine (i - j) f xs

-- place a fresh leaf down inside a tree appropriately
tweak :: Int -> Int -> Maybe a -> Tree a
tweak _ _ Nothing = Tip
tweak i0 j0 (Just a0) = go i0 j0 a0 where
  go :: Int -> Int -> a -> Tree a
  go i (shr -> j) a
    | i == 0 = Bin a Tip Tip
    | i <= j = Bin_ (go (i-1) j a) Tip
    | otherwise = Bin_ Tip $ go (i-j-1) j a
{-# inline tweak #-}

varTree :: Int -> Int -> Lens' (Tree a) (Maybe a)
varTree i j f Tip = tweak i j <$> f Nothing
varTree i (shr -> j) f (Bin a l r)
  | i == 0    = (\ma -> bin ma l r) <$> f (Just a)
  | i <= j   = (\l' -> Bin a l' r) <$> varTree (i-1) j f l
  | otherwise = Bin a l <$> varTree (i-j-1) j f r
varTree i (shr -> j) f (Bin_ l r)
  | i == 0    = (\ma -> bin ma l r) <$> f Nothing
  | i <= j   = (\l' -> bin_ l' r) <$> varTree (i-1) j f l
  | otherwise = bin_ l <$> varTree (i-j-1) j f r
