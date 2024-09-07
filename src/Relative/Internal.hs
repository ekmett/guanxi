{-# language DeriveTraversable #-}
{-# language TypeFamilies #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}

module Relative.Internal
  ( Unit(..)
  , Aff(..), utimes
  , Relative(..)
  , plus
  , RelativeSemigroup
  , RelativeMonoid
  , View(..) -- re-export
  , Rev(..) -- re-export
  , Cons(..)
  , Uncons(..)
  , Snoc(..)
  , Unsnoc(..)
  , Nil(..)
  , Singleton(..)
  , Q(..)
  , Cat(..)
  , pattern Nil
  , pattern Cons
  , pattern Snoc
  , foldMapQ
  , foldMapCat
  ) where

import Data.Default
import Data.Group
import Data.Semigroup (Semigroup(stimes))
import GHC.Exts as Exts hiding(One)
import Unaligned.Internal (View(..), Rev(..))

--------------------------------------------------------------------------------
-- * Interface
--------------------------------------------------------------------------------

data Unit = One | NegativeOne

instance Semigroup Unit where
  One <> x = x
  x <> One = x
  NegativeOne <> NegativeOne = One

  stimes _ One = One
  stimes e NegativeOne | even e = One
                       | otherwise = NegativeOne

instance Monoid Unit where
  mempty = One

instance Group Unit where
  invert = id
  pow = flip stimes

instance Abelian Unit

data Aff = Aff !Unit !Integer

-- group action
utimes :: Unit -> Integer -> Integer
utimes One = id
utimes NegativeOne = negate

-- a(bx+c)+d = (ab)x + ac+d
instance Semigroup Aff where
  Aff a d <> Aff b c = Aff (a<>b) (utimes a c + d)
  stimes e (Aff One x) = Aff One (toInteger e * x)
  stimes e (Aff NegativeOne x) = Aff (stimes e NegativeOne) $ if even e then 0 else x

instance Monoid Aff where
  mempty = Aff One 0

-- y = ax+b
-- y-b = ax
-- (y-b)*a^-1 = x
-- (a^-1)y-(a^-1)b = x

instance Group Aff where
  invert (Aff a b) = Aff (invert a) (negate $ invert a `utimes` b)
  pow = flip stimes

-- group action
class Relative a where
  rel :: Aff -> a -> a

instance Relative a => Relative (Maybe a) where
  rel = fmap . rel

instance Relative Integer where
  rel (Aff a b) x = utimes a x + b

plus :: Relative a => a -> Integer -> a
plus r n = rel (Aff One n) r

-- rel d (a <> b) = rel d a <> rel d b
class (Relative a, Semigroup a) => RelativeSemigroup a where

-- rel d mempty = mempty
class (Relative a, RelativeSemigroup a, Monoid a) => RelativeMonoid a

-- TODO: use Control.Lens.Cons?
class Cons t where
  cons :: Relative a => a -> t a -> t a

class Nil t where
  nil :: Relative a => t a

class Uncons t where
  uncons :: Relative a => t a -> View a (t a)

class Unsnoc t where
  unsnoc :: Relative a => t a -> View (t a) a

class Snoc t where
  snoc :: Relative a => t a -> a -> t a

class Singleton t where
  singleton :: Relative a => a -> t a

pattern Nil :: (Nil t, Uncons t, Relative a) => t a
pattern Nil <- (uncons -> Empty) where
  Nil = nil

pattern Cons :: (Cons t, Uncons t, Relative a) => a -> t a -> t a
pattern Cons a as <- (uncons -> a :&: as) where
  Cons a as = cons a as

pattern Snoc :: (Snoc t, Unsnoc t, Relative a) => t a -> a -> t a
pattern Snoc as a <- (unsnoc -> as :&: a) where
  Snoc as a = snoc as a

--------------------------------------------------------------------------------
-- Reversing containers
--------------------------------------------------------------------------------

instance Relative (f a) => Relative (Rev f a) where
  rel d (Rev as) = Rev (rel d as)

instance Nil t => Nil (Rev t) where
  nil = Rev nil

instance Cons t => Snoc (Rev t) where
  snoc (Rev t) f = Rev (cons f t)

instance Uncons t => Unsnoc (Rev t) where
  unsnoc (Rev t) = case uncons t of
    l :&: r -> Rev r :&: l
    Empty -> Empty

instance Unsnoc t => Uncons (Rev t) where
  uncons (Rev t) = case unsnoc t of
    l :&: r -> r :&: Rev l
    Empty -> Empty

instance Snoc t => Cons (Rev t) where
  cons a (Rev b) = Rev (snoc b a)

instance Singleton t => Singleton (Rev t) where
  singleton = Rev . singleton

--------------------------------------------------------------------------------
-- * Queues
--------------------------------------------------------------------------------

data Q a = Q {-# unpack #-} !Aff [a] (Rev [] a) [a]

instance Relative (Q a) where
  rel (Aff One 0) xs = xs
  rel d (Q d' as bs cs) = Q (d <> d') as bs cs

{-# complete Nil, Cons :: Q #-}

instance Default (Q a) where
  def = Q mempty [] (Rev []) []

instance (Show a, Relative a) => Show (Q a) where
  showsPrec d = showsPrec d . Exts.toList

instance Relative a => IsList (Q a) where
  type Item (Q a) = a
  fromList = foldr cons nil
  fromListN _ = foldr cons nil
  toList = foldMapQ pure

foldMapQ :: (Relative a, Monoid m) => (a -> m) -> Q a -> m
foldMapQ f (Q d as bs _) = foldMap (f . rel d) as <> foldMap (f . rel d) bs

instance Nil Q where
  nil = Q mempty [] (Rev []) []

instance Cons Q where
  cons a (Q d f r s) = let a' = rel (invert d) a in Q d (a':f) r (a':s)

instance Uncons Q where
  uncons (Q _ [] (Rev []) _) = Empty
  uncons (Q d (x:f) r s) = rel d x :&: exec d f r s
  uncons _ = error "Q.uncons: invariants violated"

instance Singleton Q where
  singleton a = Q mempty [a] (Rev []) []

instance Snoc Q where
  snoc (Q d f (Rev r) s) a = exec d f (Rev (rel (invert d) a : r)) s

exec :: Aff -> [a] -> Rev [] a -> [a] -> Q a
exec d xs ys (_:t) = Q d xs ys t
exec d xs ys []    = Q d xs' (Rev []) xs' where xs' = rotate xs ys []

rotate :: [a] -> Rev [] a -> [a] -> [a]
rotate [] (Rev [y]) a = y:a
rotate (x:xs) (Rev (y:ys)) a = x:rotate xs (Rev ys) (y:a)
rotate _ _ _ = error "Q.rotate: invariant broken"

--------------------------------------------------------------------------------
-- * Catenable lists
--------------------------------------------------------------------------------

data Cat a = E | C a !(Q (Cat a))

instance Relative a => Relative (Cat a) where
  rel _ E = E
  rel (Aff One 0) as = as
  rel d (C a as) = C (rel d a) (rel d as)

instance Relative a => RelativeSemigroup (Cat a)
instance Relative a => RelativeMonoid (Cat a)

instance (Relative a, Show a) => Show (Cat a) where
  showsPrec d = showsPrec d . Exts.toList

foldMapCat :: (Relative a, Monoid m) => (a -> m) -> Cat a -> m
foldMapCat _ E = mempty
foldMapCat f (C a as) = f a <> foldMapQ (foldMapCat f) as

{-# complete Nil, C #-}
{-# complete E, Cons #-}
{-# complete Nil, Cons :: Cat #-}

instance Default (Cat a) where
  def = E

instance Relative a => Semigroup (Cat a) where
  E <> xs = xs
  xs <> E = xs
  C x xs <> ys = link x xs ys

instance Relative a => Monoid (Cat a) where
  mempty = E

instance Relative a => IsList (Cat a) where
  type Item (Cat a) = a
  fromList = foldr cons nil
  fromListN _ = foldr cons nil
  toList = foldMapCat pure

link :: Relative a => a -> Q (Cat a) -> Cat a -> Cat a
link x xs ys = C x (snoc xs ys)

-- O(1+e) where e is the number of empty catenable lists in the Q
linkAll :: Relative a => Q (Cat a) -> Cat a
linkAll q = case uncons q of
  c@(C a t) :&: q' -> case uncons q' of
    Empty -> c
    _ -> link a t (linkAll q')
  E :&: q' -> linkAll q' -- recursive case in case of empty queues, unused
  Empty -> E

instance Nil Cat where
  nil = E

instance Uncons Cat where
  uncons E = Empty
  uncons (C a q) = a :&: linkAll q

instance Cons Cat where
  cons a E  = C a nil
  cons a ys = link a nil ys

instance Singleton Cat where
  singleton a = C a nil

instance Snoc Cat where
  snoc xs a = xs <> singleton a
