{-# language GADTs #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}
{-# language DeriveFunctor #-}

module Walk where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.List (unfoldr)
import Data.Ratio

-- TODO: use Unaligned.Internal.View
stream :: (s -> Maybe (o, s)) -> (s -> i -> s) -> s -> [i] -> [o]
stream f g z xs = case f z of
  Just (y, z') -> y : stream f g z' xs
  Nothing -> case xs of
    [] -> []
    x:xs -> stream f g (g z x) xs

unstream
  :: (s -> Maybe (o, s))
  -> (s -> i -> s)
  -> (s -> r -> i)
  -> (r -> o -> r)
  -> s -> r -> [i]
unstream f g h k z w = case f z of
  Just (y, z') -> unstream f g h k z' (k w y)
  Nothing | x <- h z w -> x : unstream f g h k (g z x) w

-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/arith.pdf>
-- will be useful here. This is basically just decoding without encoding
-- no?

-- * rational arithmetic encoding

type Q = Rational
data I = I Q Q
type Bit = Int -- 0 or 1 only

unit :: I
unit = I 0 1

within :: Q -> I -> Bool
within x (I l r) = l <= x && x < r

pick :: I -> Q 
pick (I l r) = (l + r) / 2

lerp :: I -> Q -> Q
lerp (I l r) p = l + (r-l)*p

instance Semigroup I where
  i <> I p q = I (lerp i p) (lerp i q)

instance Monoid I where
  mempty = I 0 1

(|>) :: I -> I -> I
(|>) = (<>)

-- recipriocal interval, for widening
inv :: I -> I
inv (I l r) = I (l/rl) (r/rl) where rl = r-l

nextBit :: I -> Maybe (Bit, I)
nextBit (I l r)
  | r <= 0.5 = Just (0, I (2*l) (2*r))
  | 0.5 <= l = Just (1, I (2*l-1) (2*r-1))
  | otherwise = Nothing


-- |
-- @
-- 'length' ('toBits' ('I' l r) <= - 'logBase' 2 (r-l)
-- @
toBits :: I -> [Bit]
toBits = unfoldr nextBit

-- |
-- @
-- 'fromBits' ('toInts' i) `'within'` i
-- @
fromBits :: [Bit] -> Q
fromBits = foldr pack 0.5 where
  pack :: Bit -> Q -> Q
  pack b x = (fromIntegral b + x) / 2

data EI = EI {-# unpack #-} !Int {-# unpack #-} !Q {-# unpack #-} !Q

lo, hi :: I -> Q
lo (I l _) = l
hi (I _ r) = r

pattern E :: Int -> I -> EI
pattern E n ilr <- EI n l (I l -> ilr) where
  E n ilr = EI n (lo ilr) (hi ilr)
 
-- reports the number of times it had to expand
expand :: EI -> EI
expand (EI n l r)
    | 0.25 <= l, r <= 0.75 = expand (EI (n+1) (2*l-0.5) (2*r-0.5))
    | otherwise = EI n l r

-- | @
-- contract . expand = contract
-- contract n (i <> j) = contract n i <> j
-- @
contract :: EI -> I
contract (EI n l r) = I (rescale n l) (rescale n r) where
  rescale n x = (x - 0.5)*2^^(-n) + 0.5

-- | @
-- contract (enarrow ei i) = contract ei |> i
-- @
enarrow :: EI -> I -> EI
enarrow ei int2 = E n (int1 <> int2) where
  E n int1 = expand ei

nextBits :: EI -> Maybe ([Bit], I)
nextBits (EI n l r)
  | r <= 0.5  = Just (bits n 0, I (2*l) (2*r))
  | 0.5 <= l  = Just (bits n 1, I (2*l-1) (2*r-1))
  | otherwise = Nothing
  
bits :: Int -> Bit -> [Bit]
bits n b = b : replicate n (1-b)

-- integral arithmetic encoding

{-
-- 64 bit numbers for intervals
data I = I {-# unpack #-} !Int64 {-# unpack #-} !Int64
data II = II {-# unpack #-} !Int64 {-# unpack #-} !Int64 {-# unpack #-} !Int64 

inarrow :: I -> II -> I
inarrow (I l r) (II p q d) = I (l + div ((r-l)*p) d) (r + div ((r-l)*q) d)

-- this belongs, morally, in a put monad instead
--encode m ei = concat . stream nextBits enarrow ei . encodeSyms m
--

decode m ei bs = unstream nextBitsM step nextSym sub (m, ei) (buffer bs)

buffer bs = (z, rs) where
  z = foldl 9\x b -> 2 * x + b) 0 cs
  (cs, rs) = splitAt e (bs ++ 1 : replicate (e - 1) 0)

nextSym (m, ei) (z, rs) = decodeSym m t where
  t = ((k-l+1)* denom m - 1) `div` (r -l)
  k = fscale n (z,rs)
  (n,(l,r)) = expand ei

-- here we don't want to read a bit, we want a random bit and to build the history tape
sub = foldl unop where
  unop (z,r:rs) b = (2*z - w*b + r, rs)

fscale (n,(z,rs)) = foldl (\x b -> 2*x + b - 32) z (take n rs)
-}

-- invariant in a
data Model a where
  Model :: (s -> a -> (I, s)) -> (s -> Q -> a) -> s -> Model a

data History = BRANCH History History | Done

open :: History
open = BRANCH open open

branch :: History -> History -> History
branch Done Done = Done
branch x y = Branch x y

pattern Branch :: History -> History -> History
pattern Branch x y <- BRANCH x y where
  Branch x y = branch x y

newtype M m a = M
  { runM :: m Bool -> (a -> History -> m History) -> History -> m History
  } deriving Functor

instance Applicative (M m) where
  pure a = M $ \ _ k -> k a
  (<*>) = ap

instance Monad (M m) where
  m >>= f = M $ \ rand k -> runM m rand $ \ a -> runM (f a) rand k

instance Monad m => Alternative (M m) where
  empty = M $ \ _ _ _ -> pure Done -- ignore the history and continuation
  -- would it be better to explicitly manage a zipper?
  l <|> r = M $ \ rand k h -> case h of
    Done -> pure Done
    Branch a Done -> (\a' -> Branch a' Done) <$> runM l rand k a
    Branch Done b -> (\b' -> Branch Done b') <$> runM r rand k b
    Branch a b -> do
      c <- rand
      if c then (\a' -> Branch a' b) <$> runM l rand k a
      else (\b' -> Branch a b') <$> runM r rand k b

instance Monad m => MonadPlus (M m) where
  mplus = (<|>) 
  mzero = empty

{-
invariant: Branch Done Done = Done

with the idea that these can be used to help inform a 
-}

