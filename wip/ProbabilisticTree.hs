{-# language TypeFamilies, DeriveFoldable #-}
-- probabilistic trees
module Prob 
  ( Tree
  , singleton
  ) where

-- I want to take a hash function for elements, and produce a tree

import Data.Bits
import Data.Foldable as F
import Data.Hashable
import Data.Word
import GHC.Exts

-- compare level only
lts :: Word -> Word -> Bool
lts a b = a < b && a < xor a b

-- pugh style probabilistic trees with cached hash values
--
-- this is very similar to a treap.
data Tree a
  = Tip
  | Leaf a {-# unpack #-} !Word
  | Bin !(Tree a) a {-# unpack #-} !Word !(Tree a)
  deriving (Show, Eq, Foldable)

instance Semigroup (Tree a) where
  Tip <> a = a 
  a <> Tip = a
  n0@(Leaf a h) <> n1 = go n1 where
    go Tip = n0 -- before a i n
    go n@(Leaf b i)
      | h `lts` i = Bin n0 b i Tip
      | otherwise = Bin Tip a h n
    go n@(Bin l b i r)
      | h `lts` i = Bin (go l) b i r
      | otherwise = Bin Tip a h n
  n0 <> n1@(Leaf b i) = go n0 where
    go Tip = n1
    go n@(Leaf a h)
      | h `lts` i = Bin n b i Tip
      | otherwise = Bin Tip a h n1
    go n@(Bin l a h r)
      | h `lts` i = Bin n b i Tip
      | otherwise = Bin l a h (go r)
  x@(Bin l a h r) <> y@(Bin l' b i r') -- this could be deconstructed a _little_ more to avoid full <>'s below
    | h `lts` i = Bin (x <> l') b i r'
    | otherwise = Bin l a h (r <> y)

instance Monoid (Tree a) where
  mempty = Tip
  
singleton :: Hashable a => a -> Tree a
singleton a = Leaf a (fromIntegral (hash a))

instance Hashable a => IsList (Tree a) where
  type Item (Tree a) = a
  fromList = foldl (\t a -> after t a (fromIntegral (hash a))) Tip where
    after :: Tree a -> a -> Word -> Tree a
    after Tip b i = Leaf b i
    after n@(Leaf a h) b i
      | h `lts` i = Bin n b i Tip
      | otherwise = Bin Tip a h (Leaf b i)
    after n@(Bin l a h r) b i
      | h `lts` i = Bin n b i Tip
      | otherwise = Bin l a h (after r b i)
  toList = F.toList

{-
-- extract the leftmost element of the tree

data Minimal a = NoMin | Minimal a {-# unpack #-} !Word !(Tree a)

minimal :: Tree a -> Minimal a
minimal Tip = NoMin
minimal (Leaf a h) = Minimal a h Tip
minimal (Bin l a h r) = case minimal l of
  NoMin -> Minimal a h r
  Minimal a' h' l' -> Minimal a' h' (Bin l' a h r)

-- extract the rightmost element of the tree

data Maximal a = NoMax | Maximal !(Tree a) a {-# unpack #-} !Word

maximal :: Tree a -> Maximal a
maximal Tip = NoMax
maximal (Leaf a h) = Maximal Tip a h
maximal (Bin l a h r) = case maximal r of
  NoMax -> Maximal l a h
  Maximal r' a' h' -> Maximal (Bin l a h r') a h
-}

