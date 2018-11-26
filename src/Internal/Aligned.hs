{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Type-aligned sequences in the style of Atze van der Ploeg's 
-- <http://okmij.org/ftp/Haskell/zseq.pdf Reflection without Remorse>

module Internal.Aligned
  ( View(..)
  , Cons(..)
  , Uncons(..)
  , Snoc(..)
  , Unsnoc(..)
  , Nil(..)
  , Op(..)
  , Thrist(..)
  , Q(..)
  , Cat(..)
  , Rev(..)
  , pattern Cons
  , pattern Snoc
  , pattern Nil
  ) where

import Prelude hiding (id,(.))
import Control.Category
import Data.Kind

--------------------------------------------------------------------------------
-- * Interface
--------------------------------------------------------------------------------

data View l r a b where
  (:&:) :: l b c -> r a b -> View l r a c
  Empty :: View l r a a

class Cons t where
  cons :: f b c -> t f a b -> t f a c

class Nil t where
  nil :: t f a a

class Uncons t where
  uncons :: t f a b -> View f (t f) a b

class Unsnoc t where
  unsnoc :: t f a b -> View (t f) f a b

class Snoc t where
  snoc :: t f b c -> f a b -> t f a c

class Singleton t where
  singleton :: f a b -> t f a b
 
pattern Cons :: (Cons t, Uncons t) => f b c -> t f a b -> t f a c
pattern Cons a as <- (uncons -> a :&: as) where
  Cons a as = cons a as

pattern Snoc :: (Snoc t, Unsnoc t) => t f b c -> f a b -> t f a c
pattern Snoc as a <- (unsnoc -> as :&: a) where
  Snoc as a = snoc as a

pattern Nil :: (Nil t, Uncons t) => (a~b) => t f a b
pattern Nil <- (uncons -> Empty) where
  Nil = nil 

--------------------------------------------------------------------------------
-- * The opposite category
--------------------------------------------------------------------------------

newtype Op (f :: k -> k -> Type) (a :: k) (b :: k) = Op { runOp :: f b a }

instance Category f => Category (Op f) where
  id = Op id
  Op f . Op g = Op (g . f)

--------------------------------------------------------------------------------
-- * Reversing containers
--------------------------------------------------------------------------------

newtype Rev t f a b = Rev { runRev :: t (Op f) b a }

instance Category (t (Op f)) => Category (Rev t f) where
  id = Rev id
  Rev f . Rev g = Rev (g . f)

instance Nil t => Nil (Rev t) where
  nil = Rev nil

instance Cons t => Snoc (Rev t) where
  snoc (Rev t) f = Rev (cons (Op f) t)

instance Uncons t => Unsnoc (Rev t) where
  unsnoc (Rev t) = case uncons t of
    Op l :&: r -> Rev r :&: l
    Empty -> Empty

instance Unsnoc t => Uncons (Rev t) where
  uncons (Rev t) = case unsnoc t of
    l :&: Op r -> r :&: Rev l
    Empty -> Empty

instance Snoc t => Cons (Rev t) where
  cons a (Rev b) = Rev (snoc b (Op a))

instance Singleton t => Singleton (Rev t) where
  singleton = Rev . singleton . Op

--------------------------------------------------------------------------------
-- * Thrists
--------------------------------------------------------------------------------

data Thrist f a b where
  Id   :: Thrist f a a
  (:.) :: f b c -> !(Thrist f a b) -> Thrist f a c

{-# complete Nil, Cons :: Thrist #-}
{-# complete Id , Cons :: Thrist #-}
{-# complete Nil, (:.) :: Thrist #-}

instance Category (Thrist f) where
  id = Id
  xs . Id = xs
  Id . ys = ys
  (x :. xs) . ys = x :. (xs . ys)

instance Nil Thrist where
  nil = Id

instance Cons Thrist where
  cons = (:.)

instance Uncons Thrist where
  uncons (a :. b) = a :&: b
  uncons Id = Empty

instance Singleton Thrist where
  singleton a = a :. Id

--------------------------------------------------------------------------------
-- * Queues
--------------------------------------------------------------------------------

data Q f a b where
  Q :: !(Thrist f b c) -> !(Rev Thrist f a b) -> !(Thrist f b x) -> Q f a c

{-# complete Nil, Cons :: Q #-}

instance Nil Q where
  nil = Q nil nil nil

instance Cons Q where
  cons a (Q f r s) = Q (a :. f) r (undefined :. s)

instance Uncons Q where
  uncons (Q Id (Rev Id) _) = Empty
  uncons (Q (x :. f) r s) = x :&: exec f r s
  uncons _ = error "Q.uncons: invariants violated"

instance Singleton Q where
  singleton a = Q (singleton a) nil nil

instance Snoc Q where
  snoc (Q f r s) a = exec f (snoc r a) s

exec :: Thrist f b c -> Rev Thrist f a b -> Thrist f b x -> Q f a c
exec xs ys (_ :. t) = Q xs ys t
exec xs ys Id        = Q xs' nil xs' where xs' = rotate xs ys nil

rotate :: Thrist f c d -> Rev Thrist f b c -> Thrist f a b -> Thrist f a d
rotate Id (Rev (Op y :. Id)) a = y :. a
rotate (x :. xs) (Rev (Op y :. ys)) a = x :. rotate xs (Rev ys) (y :. a)
rotate _ _ _ = error "Q.rotate: invariant broken"

--------------------------------------------------------------------------------
-- * Catenable lists
--------------------------------------------------------------------------------

data Cat f a b where
  E :: Cat f a a 
  C :: f b c -> !(Q (Cat f) a b) -> Cat f a c

{-# complete Nil, Cons :: Cat #-}
{-# complete E  , Cons :: Cat #-}
{-# complete Nil, C    :: Cat #-}

instance Category (Cat f) where
  id = E

  E . xs = xs
  xs . E = xs
  C x xs . ys = link x xs ys

link :: f c d -> Q (Cat f) b c -> Cat f a b -> Cat f a d
link x xs ys = C x (snoc xs ys)

-- O(1+e) where e is the number of empty catenable lists in the Q
linkAll :: Q (Cat f) a b -> Cat f a b
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
  snoc xs a = xs . singleton a

