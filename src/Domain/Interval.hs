{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Domain.Interval where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Numeric.Natural
import Ref
import Signal

-- Todo: union-find to add more efficient equality constraints

data Interval m
  = Concrete !Natural
  | Interval !(RefM m Natural) !(RefM m (Maybe Natural))
             !(Signal m) !(Signal m)

instance HasSignals m (Interval m) where
  signals Concrete{} = mempty
  signals (Interval _ _ sl sh) = signals sl <> signals sh

abstract :: Natural -> Interval m
abstract = Concrete

-- attempt to refine this interval without necessarily concretizing it
-- this imitates a 'doubling' open search strategy search
refine :: MonadSignal e m => Interval m -> m Bool
refine (Concrete _) = pure False
refine (Interval rl rh sl sh) = readRef rh >>= \case
  Nothing -> readRef rl >>= \n ->
        True <$ writeRef rh (Just $ 2*n) <* fire sh -- both sides refined
    <|> True <$ writeRef rl (2*n+1) <* fire sl
  Just h -> readRef rl >>= \l ->
    let m = l + div (h - l) 2; lm = m - 1
     in (l < h) <$ guard (l <= lm) <* writeRef rh (Just lm) <* fire sh
    <|> (l < h) <$ guard (m <= h)  <* writeRef rl m <* fire sl

concrete :: MonadSignal e m => Interval m -> m Natural
concrete (Concrete a) = pure a
concrete x@(Interval rl rh sl sh) = readRef rh >>= \case
  Just m -> readRef rl >>= \case
    n -> go n where
      go !i = scope (do
           guard (i <= m)
           writeRef rl i
           when (i /= n) (fire sl)
           writeRef rh (Just i)
           when (i /= m) (fire sh)
           pure i)
       <|> go (i+1)
  -- danger, infinite result set!
  Nothing -> readRef rl >>= go where
    go !i = do
            writeRef rl i
            writeRef rh $ Just i
            i <$ fire x
        <|> go (i+1)

fresh :: MonadSignal e m => m (Interval m)
fresh = interval 0 Nothing

interval :: MonadSignal e m => Natural -> Maybe Natural -> m (Interval m)
interval n (Just m) | n == m = pure $ Concrete n
interval n mm = do
  rl <- newRef n
  rh <- newRef mm
  sl <- newSignal_
  sh <- newSignal (void . concrete . Interval rl rh sl)
    -- this scheme is hazardous if i later prune the network by looking for only those
    -- that are reachable from those with grounding strategies before grounding
    -- in which case, we'd need to claim newSignal for both.
  pure $ Interval rl rh sl sh

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

{-
add :: Interval m -> Interval m -> m (Interval m)

ne :: MonadSignal e m => Interval m -> Interval m -> m ()
ne i (Concrete b) = neNatural i b
ne (Concrete a) i = neNatural a i 
ne (Interval 
-}
