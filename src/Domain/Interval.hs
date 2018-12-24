{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiWayIf #-}
{-# language ViewPatterns #-}

module Domain.Interval where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Numeric.Natural
import Ref
import Signal
import Relative.Base as R

type C m = RefM m (Content m)

-- interval propagators
data P m = P {-# unpack #-} !Z (Z -> C m -> m ())

instance Relative (P m) where
  rel d (P d' f) = P (d+d') f

runP :: P m -> C m -> m ()
runP (P d f) c = f d c

type Z = Integer
data R m = R {-# unpack #-} !Int !(Maybe Z) !(Maybe Z) !(R.Cat (P m)) !(R.Cat (P m))

data Content m
  = Root {-# unpack #-} !(R m)
  | Child !Z {-# unpack #-} !(C m)

instance Relative (Content m) where
  rel d (Root rank lo hi ps) = Root rank (rel d lo) (rel d hi) (rel d ps)
  rel d (Child d' z) = Child (d + d') z

data Interval m
  = Concrete !Z
  | Interval !Z {-# unpack #-} !(C m)

findRef :: MonadRef m => C m -> m (Z, R m, C m)
findRef c = readRef c >>= \case
  Root r -> pure (0,r,c)
  Child z c' -> do
    ((+) z -> z'',r,root) <- findEx c'
    (z'', r, root) <$ writeRef c (Child z'' root)

maxMay :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMay mx my = liftA2 max mx my <|> mx <|> my

minMay :: Ord a => Maybe a -> Maybe a -> Maybe a
minMay mx my = liftA2 min mx my <|> mx <|> my

-- union x y d means x := y + d
unionRef :: MonadRef m => C m -> C m -> Z -> m ()
unionRef x y d = do
  (xd, R xrank xlo xhi xlop xhip, xroot) <- findRef x -- x := [xlo + xd .. xhi + xd ]
  (yd, R yrank ylo yhi ylop yhip, yroot) <- findRef y -- y := [ylo + yd .. yhi + yd ]
  -- [xlo + xd .. xhi + xd] = [ylo + yd + d .. yhi + yd + d]
  -- xlo + xd = ylo + yd + d
  -- xlo + xd - d = ylo + yd
  -- xlo + xd - d - yd = ylo
  if xrank < yrank then do
    writeRef xroot $ Child d yroot
    let offset = xd - d - yd
        ylo' = maxMay (fmap (+offset) xlo) ylo
        yhi' = minMay (fmap (+offset) xhi) yhi
    for_ ylo' $ \ a -> for_ yhi' $ \b -> guard (a <= b) -- abort on empty intervals
    writeRef yroot $ Root $ R yrank ylo' yhi' (rel (-d) xlop <> ylop) (rel (-d) xhip <> yhip)
    when (ylo' > ylo) $ runP ylop yroot
    when (yhi' < yhi) $ runP yhip yroot
  else do
    writeRef yroot $ Child (-d) xroot
    -- xlo + xd = ylo + yd + d
    -- xlo = ylo + yd + d - xd
    let offset = yd + d - xd
        xlo' = maxMay (fmap (+offset) ylo) xlo
        xhi' = minMay (fmap (+offset) yhi) xhi
        xrank'
          | xrank == yrank = xrank + 1
          | otherwise      = xrank
    for xlo' $ \a -> for_ xhi' $ \b -> guard (a <= b) -- abort on empty intervals
    writeRef xroot $ Root $ R xrank' xlo' xhi' (xlop <> rel d ylop) (xhip <> rel d yhip)
    when (xlo' > xlo) $ runP xlop xroot
    when (xhi' < xhi) $ runP xhip xroot

{-
-- TODO: This would need HasSignals to be lifted into m. Quite doable.
instance HasSignals m (Interval m) where
-}

-- attempt to refine this interval without necessarily concretizing it
-- this imitates a 'doubling' open search strategy search
-- TODO: Finalization should round-robin like this around the variables
-- even in FDVar, eliminating the need for interleaving
refineRef :: MonadRef m => C m -> m (Maybe Z)
refineRef c = do
  (d, R rank mlo mhi lop hip, root) <- findRef c
  -- let negLo = maybe True (<0) mlo
  --    posHi = maybe True (>=0) mhi
  fmap (fmap (+d)) $ if
    | Just lo <- mlo, Just hi <- mhi, m <- div (lo + hi) 2 -- we have a finite interval, split it
      -> if lo == hi then pure (Just lo) -- single value, can't refine
         else do let pm = m-1
                 writeRef root $ Root $ R rank mlo (Just pm) lop hip
                 guard (pm >= lo)
                 when (pm < hi) (runP hip root)
                 pure $ guard (pm == lo) *> mlo
          <|> do let progress = m > lo
                 writeRef root $ Root $ R rank (Just m) mhi lop hip
                 guard (m <= hi)
                 when (m > lo) $ runP lop root
                 pure $ guard (m == hi) *> mhi
    -- TODO: the next two cases should be able to merge to reduce duplication
    | Just lo <- mlo, lo >=0, Nothing <- mhi, mid <- 2*lo -- non-negative, split using doubling open ended search scheme
        -> do writeRef root $ Root $ R rank mlo (Just mid) lop hip
              runP hip root -- we went from an infinite upper bound to a finite upper bound, definite progress
              pure $ guard (lo == mid) *> mlo
       <|> do writeRef root $ Root $ R rank (Just (mid+1)) mhi lop hip
              runP lop root -- lo >= 0 ==> 2*lo+1 > lo, so we made progress
              pure $ guard (hi == mid+1) *> mhi
    | Nothing <- mlo, Just hi <- mhi, hi < 0, mid <- 2*hi
        -> do writeRef root $ Root $ R rank (Just (mid+1)) mhi lop hip -- hi<0 ==> 2*hi+1<=hi
              runP lop root
              pure $ guard (hi == mid+1) *> mhi
       <|> do writeRef root $ Root $ R rank mlo (Just mid) lop hip -- h<0 ==> 2*hi<hi
              runP hip root
              pure $ guard (lo == mid) *> mlo
    | otherwise -- mixed negative and positive, split arbitrarily at 0, TODO check for finiteness first
        -> do writeRef root $ Root $ R rank mlo (Just (-1)) lop hip
              runP hip root
              pure $ guard (mlo == Just (-1)) *> mlo
       <|> do writeRef root $ Root $ R rank (Just 0) mhi lop hip
              runP lop root
              pure $ guard (Just 0 == mhi) *> mhi

abstract :: Z -> Interval m
abstract = Concrete

-- reduce to a single natural, incrementally
concrete :: MonadSignal e m => Interval m -> m Natural
concrete (Concrete a) = pure a
concrete (Interval d r) = loop where
  loop = refineRef r >>= \case
    Nothing -> loop
    Just z -> pure (d+z)

-- bottom = [-infinity,infinity], a fresh interval
bottom :: MonadSignal e m => m (Interval m)
bottom = interval Nothing Nothing

interval :: MonadSignal e m => Maybe Integer -> Maybe Integer -> m (Interval m)
interval (Just lo) (Just hi)
  | lo == hi = pure $ Concrete lo -- one value, nothing will ever propagate
  | lo > hi  = empty -- empty interval, fail now
interval mlo mhi = do
  r <- newRef (Root $ R 0 mlo mhi mempty mempty) -- build the interval
  -- TODO: add topological sort on grounding order, maybe by adding one grounding 'manager' for intervals, and subscribing to it?
  grounding $ do -- to ground this, we check if anything will be propagated out
    (_, R _ _ _ lop hip, root) <- findRef r
    unless (null lop && null hip) $ fix $ \loop -> -- ground this if anybody is listening
      refineRef root >>= \case
        Nothing -> loop -- TODO: only bother to refine the side that we care about?
        Just z -> pure ()
  pure $ Interval 0 r

addZ :: Integer -> Interval m -> Interval m
addZ n (Concrete m) = Concrete (n + m)
addZ n (Interval m c) = Interval (n + m) c

-- check if already concrete, but do not force any decisions yet
tryConcrete :: MonadSignal e m => C m -> m (Either (Z, R m, C m) Natural)
tryConcrete c = go <$> findEx c where
  go (d, R _ (Just lo) (Just hi) _ _, croot) | lo == hi = Right (d + lo)
  go l = Left l

-- current range
range :: MonadSignal e m => C m -> m (Maybe Z, Maybe Z)
range c = findRef c <&> \ (cd', R _ mlo mhi _ _, _) -> (fmap (+cd') mlo, fmap (+cd') mhi)

-- TODO: make this a three way relationship
add :: Interval m -> Interval m -> m (Interval m)
add (Concrete n) (Concrete m)   = pure $ Concrete (n + m)
add (Concrete n) (Interval m c) = pure $ Interval (n + m) c
add (Interval n c) (Concrete m) = pure $ Concrete (n + m) c
add (Interval n c) (Interval m d) =
  tryConcrete c >>= \case
    Right o -> Interval (n + m + o) d
    Left (cd, R crank clo chi clop chip, croot) -> tryConcrete d >>= \case
      Right o -> Interval (n + m + o) c
      Left (dd, R drank dlo dhi dlop dhip, droot) -> do
        result@(Interval z e) <- addZ (n + m + cd + dd) <$> interval (liftA2 (+) clo dlo) (liftA2 (+) chi dhi)
        let hup = P 0 $ do
              (_,mh1) <- range croot
              (_,mh2) <- range droot
              (ed, R erank elo ehi elop ehip, eroot) <- findRef e
              for_ (liftA2 (\h1 h2 -> h1 + h2 - ed) mh1 mh2) $ \h ->
                when (maybe True (> h) ehi) $ do
                  writeRef eroot $ Root $ R erank elo (Just h) elop ehip
                  runP ehip eroot
            lup = P 0 $ do
              (ml1,_) <- range croot
              (ml2,_) <- range droot
              (ed, R erank elo ehi elop ehip, eroot) <- findRef e
              for_ (liftA2 (\l1 l2 -> l1 + l2 - ed) ml1 ml2) $ \l ->
                when (maybe True (< l) eli) $ do
                  writeRef eroot $ Root $ R erank (Just l) ehi elop ehip
                  runP elop eroot
        writeRef croot $ Root $ R crank clo chi (snoc clop lup) (snoc chip hup)
        writeRef droot $ Root $ R drank dlo dhi (snoc dlop lup) (snoc dhip hup)
        -- TODO: add relationships back to the other two intervals via subtraction so that when this is narrowed, we can narrow them as well
        -- TODO: add slow-wakeup (when we learn things aren't bottom) and slow-sleep when we learn they are concrete
        return result

{-
  -- check for concreteness

le :: MonadSignal e m => Interval m -> Interval m -> m ()
le (Concrete a) (Concrete b) = guard (a <= b)
le (Concrete i) (Interval rl rh sl _) = do
  mh <- readRef rh
  for_ mh $ \m -> guard (i <= m)
  n <- readRef rl
  when (i > n) $ do
    writeRef rl i
    fire sl
le (Interval rl rh _ sh) (Concrete i) = do
  n <- readRef rl
  guard (i >= n)
  mh <- readRef rh
  for_ mh $ \m -> when (i < m) $ do
    writeRef rh (Just i)
    fire sh
le (Interval rl1 rh1 sl1 sh1) (Interval rl2 rh2 sl2 sh2) = do
  propagate sh2 sh1 $ do
    mh2 <- readRef rh2
    for_ mh2 $ \h2 ->
      readRef rh1 >>= \case
         Just h1 | h1 <= h2 -> pure () -- we already know better
         _ -> do -- improve upper bound
           l1 <- readRef rl1
           guard (l1 <= h2)
           writeRef rh1 mh2
           fire sh1
  propagate sl1 sl2 $ do
    l1 <- readRef rl1
    l2 <- readRef rl2
    when (l1 > l2) $ do
      mh2 <- readRef rh2
      for_ mh2 $ \h2 -> guard (l1 <= h2)
      writeRef rl2 l1
      fire sl2

-- condition asserting an interval _is_ a given natural number
eqNatural :: MonadSignal e m => Interval m -> Natural -> m ()
eqNatural (Concrete a) b = guard (a == b)
eqNatural (Interval rl rh sl sh) b = scope $ do
  l <- readRef rl
  guard (l <= b)
  when (l < b) $ writeRef rl b *> fire sl
  readRef rh >>= \case
    Just h -> do
      guard (b <= h)
      when (b < h) $ writeRef rl b *> fire sh
    Nothing -> writeRef rh (Just b) *> fire sh

eq :: MonadSignal e m => Interval m -> Interval m -> m ()
eq a (Concrete b) = eqNatural a b
eq (Concrete a) b = eqNatural b a
eq x y = le x y *> le y x
  -- TODO: assert sameness more directly by adding a union-find layer
  -- should this layer move up into Signal?

neNatural :: MonadSignal e m => Interval m -> Natural -> m ()
neNatural (Concrete a) b = guard (a /= b)
neNatural i b
    = guard (b > 0) *> le i (Concrete $ b-1)
  <|> le (Concrete $ b+1) i


ne :: MonadSignal e m => Interval m -> Interval m -> m ()
ne i (Concrete b) = neNatural i b
ne (Concrete a) i = neNatural a i
ne (Interval
-}
