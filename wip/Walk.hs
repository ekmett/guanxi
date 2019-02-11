import Data.Ratio

-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/arith.pdf>
-- will be useful here. This is basically just decoding without encoding
-- no?

-- invariant in a
data Model a where
  Model :: (s -> a -> (Interval, s)) -> (s -> Fraction -> a) -> s -> Model a

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
lerp (Interval l r) p = l + (r-l)*p

instance Semigroup I where
  i <> I p q = I (lerp i p) (lerp i q)

instance Monoid Interval where
  mempty = I 0 1

(|>) :: Interval -> Interval -> Interval
(|>) = (<>)

-- recipriocal interval, for widening
inv :: Interval -> Interval
inv (I l r) = I (l/rl) (r/rl) where rl = r-l

nextBit :: Interval -> Maybe (Bit, Interval)
nextBit (I l r)
  | r <= 0.5 = Just (0, I (2*l) (2*r))
  | 0.5 <= l = Just (1, I (2*l-1) (2*r-1))
  | otherwise = Nothing

pack :: Q -> Bit -> Q
pack b x = (b + fromIntegral x) / 2

toBits :: Interval -> [Bit]
toBits = unfoldr nextBit

fromBits :: [Bit] -> Fraction
fromBits = foldr pack 0.5

{-
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
  } 

instance Applicative (M m) where
  pure a = M $ \ _ k -> k a
  (<*>) = ap

instance Monad (M m) where
  m >>= f = M $ \ rand k -> runM m rand $ \ a -> runM (f a) rand k

instance Alternative (M m) where
  empty = done
  -- would it be better to explicitly manage a zipper?
  l <|> r = M $ \ rand k h -> case h of
    Done -> pure Done
    Branch a Done -> (\a' -> Branch a' Done) <$> runM l rand k a
    Branch Done b -> (\b' -> Branch Done b') <$> runM r rand k b
    Branch a b -> do
      b <- rand
      if b then (\a' -> Branch a' b) <$> runM l rand k a
      else (\b' -> Branch a b') <$> runM r rand k b

instance MonadPlus (M m) where
  mplus = (<|>) 
  mzero = empty
-}

{-
invariant: Branch Done Done = Done

with the idea that these can be used to help inform a 
-}
