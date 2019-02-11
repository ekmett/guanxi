{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiWayIf #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}

module Domain.Interval
  ( Interval, Z
  , abstract, concrete
  , refine
  , interval
  , bottom
  , signumi
  , negatei
  , absi
  -- relations
  , lt,  le,  eq,  ne,  ge,  gt
  , zlt, zle, zeq, zne, zge, zgt
  , ltz, lez, eqz, nez, gez, gtz
  , onceBoundedBelow, onceBoundedAbove
  , onceKnown
  -- , poly
  , onHi, onLo
  , deltaHi, deltaLo
  , known
  ) where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Cont
import Control.Monad.Fix
import Data.Bifunctor
import Data.Functor
import Data.Foldable
import Data.Monoid
import Group
import Ref
import Signal
import Relative.Internal as R

type C m = RefM m (Content m)

-- interval propagators
data P r m = P {-# unpack #-} !Aff (Aff -> r -> m ())

instance Relative (P r m) where
  rel d (P d' f) = P (d <> d') f

runP :: P r m -> r -> m ()
runP (P d f) c = f d c

-- TODO: use a map from a unique to an interval propagator
-- this means giving us disjoint union with stable names?
-- or baking the position info into the propagator so it can delete itself?
-- this implies all the hell of a typical subscription service
type Ps m = R.Cat (P (C m) m)
type Ks m = R.Cat (P Z m)

runPs, runKs :: Applicative m => R.Cat (P r m) -> r -> m ()
runPs ps root = getAp $ foldMapCat (\p -> Ap $ runP p root) ps
runKs = runPs

type Z = Integer

data R m = R
  { _rrank :: {-# unpack #-} !Int -- rank for union-find
  , rlo   :: !(Either (Ps m) Z) -- lo -- either something waiting as a one-shot for this to become something other than negative infinity or a value
  , rhi   :: !(Either (Ps m) Z) -- hi -- either something waiting as a one-shot for this to become something other than positive infinity or a value
  , rlop  :: !(Ps m) -- lo propagators -- these are removed after we go down to an interval of size one -- TODO: allow these to turn themselves off earlier
  , rhip  :: !(Ps m) -- hi propagators -- these are removed after we go down to an interval of size one -- TODO: allow these to turn themselves off earlier
  , rcov  :: !(Ks m) -- once fully known propagators
  }

instance Relative (R m) where
  rel d (R rank lo hi lop hip cov) = R rank (bimap (rel d) (rel d) lo) (bimap (rel d) (rel d) hi) (rel d lop) (rel d hip) (rel d cov)

-- intelligently strip propagators if covered
mkR :: Int -> Either (Ps m) Z -> Either (Ps m) Z -> Ps m -> Ps m -> Ks m -> R m
mkR r (Right x) (Right y) _ _ _ | x == y = R r (Right x) (Right y) nil nil nil
mkR r mlo mhi lop hip cov = R r mlo mhi lop hip cov

-- is this concrete?
covered :: R m -> Maybe Z
covered (R _ (Right x) (Right y) _ _ _) | x == y = Just x
covered _ = Nothing

{-
pattern Covered :: Z -> R m
pattern Covered z <- (covered -> Just z) where
  Covered z = R 0 (Right z) (Right z) nil nil nil
-}

-- x = y + c

-- x = ay + c | a \in {-1,1}
-- ay = x - c
-- y = (x - c)/a
--
-- y = a^-1x - a^-1c
--
-- a(cx+d)+b = (ac)x+(ad+b)

data Content m
  = Root {-# unpack #-} !(R m) -- we are the root
  | Child {-# unpack #-} !Aff {-# unpack #-} !(C m) -- union-find child, with parent pointer and an integer offset from the parent


instance Relative (Content m) where
  rel d (Root r) = Root (rel d r)
  rel d (Child d' z) = Child (d <> d') z

-- non-relational intervals, made relational by adding propagators to push information around
-- and working in a non-deterministic setting
data Interval m
  = Concrete !Z
  | Interval {-# unpack #-} !Aff {-# unpack #-} !(C m)

findRef :: MonadRef m => C m -> m (Aff, R m, C m)
findRef c = readRef c >>= \case
  Root r -> pure (mempty,r,c)
  Child z c' -> do
    (z', r, root) <- findRef c'
    let !z'' = z <> z'
    (z'', r, root) <$ writeRef c (Child z'' root)

-- bound below by a constant
zlt, zle, zeq, zne, zge, zgt :: MonadRef m => Z -> Interval m -> m ()
zle lo (Concrete a) = guard (lo <= a)
zle b (Interval m c) = do
  (d, R rank olo ohi lop hip cov, croot) <- findRef c
  let md = m<>d
  case md of
    Aff One _ -> do
      let lo' = rel (inv md) b
      for_ ohi $ \hi -> guard (lo' <= hi) -- check we're not empty
      case olo of
        Left ps -> do
          writeRef croot $ Root $ mkR rank (Right lo') ohi lop hip cov
          runPs (lop <> ps) croot
          case ohi of
            Right x | x == lo' -> runKs cov lo'
            _ -> pure ()
        Right lo2 -> when (lo' > lo2) $ do
          writeRef croot $ Root $ mkR rank (Right lo') ohi lop hip cov
          runPs lop croot
          case ohi of
            Right x | x == lo' -> runKs cov lo'
            _ -> pure ()
    Aff NegativeOne _ -> do
      let hi' = rel (inv md) b
      for_ olo $ \lo -> guard (lo <= hi') -- check we're not empty
      case ohi of
        Left qs -> do
          writeRef croot $ Root $ mkR rank olo (Right hi') lop hip cov
          runPs (hip <> qs) croot
          case olo of
            Right x | x == hi' -> runKs cov hi'
            _ -> pure ()
        Right hi2 -> when (hi' < hi2) $ do
          writeRef croot $ Root $ mkR rank olo (Right hi') lop hip cov
          runPs hip croot
          case olo of
            Right x | x == hi' -> runKs cov hi'
            _ -> pure ()

zgt = flip ltz
zge = flip lez
zlt z i = zle (z+1) i
zeq = flip eqz
zne = flip nez

-- bound above by a constant
ltz, lez, gez, gtz :: MonadRef m => Interval m -> Z -> m ()
lez i hi = zle (-hi) (neg i)
ltz i z = lez i (z-1)
gtz = flip zlt
gez = flip zle

-- eqz i z = do zli z i; lez i z
eqz :: MonadRef m => Interval m -> Z -> m ()
eqz (Concrete a) b = guard (a == b)
eqz (Interval m c) b = do
  (d, R rank olo ohi lop hip cov, croot) <- findRef c
  let md = m<>d
  let b' = rel (inv md) b
  ps <- case olo of
    Right lo -> case compare lo b' of
      LT -> pure lop
      EQ -> pure mempty
      GT -> empty
    Left ps -> pure $ ps <> lop
  qs <- case ohi of
    Right hi -> case compare b' hi of
      LT -> pure hip
      EQ -> pure mempty
      GT -> empty
    Left qs -> pure $ qs <> hip
  let rb = Right b'
  writeRef croot $ Root $ mkR rank rb rb nil nil nil
  runPs (ps<>qs) croot
  runKs cov b'

nez :: MonadRef m => Interval m -> Z -> m ()
nez (Concrete b) z = guard (b /= z)
nez i@Interval{} z = zle (z+1) i <|> lez i (z-1)

-- here i need to merge these cases
onceBoundedBelow :: MonadRef m => Interval m -> m () -> m ()
onceBoundedBelow Concrete{} m = m
onceBoundedBelow (Interval n c) m = do
  (d, r, croot) <- findRef c
  let nd = n <> d
  case nd of
    Aff One _ -> case rlo r of
      Left ps -> writeRef croot $ Root r { rlo = Left $ ps `R.snoc` P mempty (\_ _ -> m) }
      Right{} -> m -- already bounded below
    Aff NegativeOne _ -> case rhi r of
      Left qs -> writeRef croot $ Root r { rhi = Left $ qs `R.snoc` P mempty (\_ _ -> m) }
      Right{} -> m -- already bounded above

onceBoundedAbove :: MonadRef m => Interval m -> m () -> m ()
onceBoundedAbove = onceBoundedBelow . neg

-- TODO: merge lo/hi code from here down

onLo, onHi :: MonadRef m => Interval m -> (Z -> m ()) -> m () -- every improvement to the lower bound triggers this, including the initial non-bottom change
onLo (Concrete a) f = f a
onLo (Interval n c) f = do
  (d, r, croot) <- findRef c
  let nd = n<>d
  case covered r of
    Just (rel nd -> z) -> f z -- done, nothing will ever improve this, nothing to watch
    Nothing -> case nd of
      Aff One _ -> do
        let f' z e = findRef e >>= \ (j, R { rlo = Right k }, _) ->
              f (rel (z<>j) k)
        writeRef croot $ Root r { rlop = rlop r `R.snoc` P nd f' }
      Aff NegativeOne _ -> do
        let f' z e = findRef e >>= \ (j, R { rhi = Right k }, _) ->
              f (rel (z<>j) k)
        writeRef croot $ Root r { rhip = rhip r `R.snoc` P nd f' }

onHi i f = onLo (neg i) (f . negate)


-- when run this will tell the improvement since the last firing
deltaLo, deltaHi :: MonadRef m => Interval m -> (Maybe Z -> Z -> m ()) -> m ()
deltaLo (Concrete a) f = f (Just a) a -- just the initial firing
deltaLo i f = do
  (mlo, _) <- range i
  omlo <- newRef mlo
  onLo i $ \lo -> do
    x <- updateRef omlo $ \a -> (a, Just lo)
    f x lo

deltaHi i f = deltaLo (neg i) $ \oldhi hi -> f (negate <$> oldhi) (negate hi)

onceKnown :: MonadRef m => Interval m -> (Z -> m ()) -> m ()
onceKnown (Concrete a) f = f a
onceKnown (Interval n c) f = do
  (d, r, croot) <- findRef c
  let nd = n<>d
  case covered r of
    Just z -> f (rel nd z)
    Nothing -> writeRef croot $ Root r { rcov = rcov r `R.snoc` P nd (\a b -> f (rel a b)) }

-- rel o xap <> xbp

maxx, minx :: Ps m -> Ps m -> Aff -> Either (Ps m) Z -> Either (Ps m) Z -> (Ps m, Either (Ps m) Z)
maxx xap xbp o (Right a) (Right b) =
  ( (if rel o a < b then rel o xap else nil) <> (if b < rel o a then xbp else nil)
  , Right $ max (rel o a) b
  )
maxx _ _ o (Left ps) (Left ps') = (nil, Left $ rel o ps <> ps')
maxx xap _ o (Left ps) rb@Right{} = (rel o (xap <> ps), rb)
maxx _ xbp o (Right a) (Left ps)  = (xbp <> ps, Right $ rel o a)

minx xap xbp o (Right a) (Right b) =
  ( (if rel o a > b then rel o xap else nil) <> (if b > rel o a then xbp else nil)
  , Right $ min (rel o a) b
  )
minx _ _ o (Left ps) (Left ps') = (nil, Left $ rel o ps <> ps')
minx xap _ o (Left ps) rb@Right{} = (rel o (xap <> ps), rb)
minx _ xbp o (Right a) (Left ps)  = (xbp <> ps, Right $ rel o a)

no :: Unit -> a -> a -> a
no One x _ = x
no NegativeOne _ y = y

-- this is a special case when we go to define 'addition' and only two of the entries are non-constant
-- and is implemented by union-find rather than propagation
-- union x f y means x := f(y), y = (f^-1)(x)
eqRef :: MonadRef m => C m -> Aff -> C m -> m ()
eqRef x d y = do
  (xd, R xrank xlo xhi xlop xhip xcov, xroot) <- findRef x
  (yd, R yrank ylo yhi ylop yhip ycov, yroot) <- findRef y
  -- xroot = t(yroot)
  let t@(Aff u _) = inv xd <> d <> yd
      o = inv t
  if xrank < yrank then do
    writeRef xroot $ Child t yroot
    let zlop = no u xlop xhip
        zhip = no u xhip xlop
        (todo,  ylo') = maxx zlop ylop o (no u xlo xhi) ylo
        (todo', yhi') = minx zhip yhip o (no u xhi xlo) yhi
        result = mkR yrank ylo' yhi'
          (ylop <> rel o zlop) (yhip <> rel o zhip) (ycov <> rel o xcov)
    writeRef yroot $ Root result
    runPs (todo <> todo') yroot
    case covered result of
      Just z -> runKs (ycov <> rel o xcov) z
      Nothing -> pure ()
  else do
    writeRef yroot $ Child o xroot
    let zlop = no u ylop yhip
        zhip = no u yhip ylop
        (todo,  xlo') = maxx zlop xlop t (no u ylo yhi) xlo
        (todo', xhi') = minx zhip xhip t (no u yhi ylo) xhi
        result = mkR (if xrank == yrank then yrank + 1 else yrank) xlo' xhi'
          (xlop <> rel t zlop) (xhip <> rel t zhip) (xcov <> rel t ycov)
    writeRef xroot $ Root result
    runPs (todo <> todo') xroot
    case covered result of
      Just z -> runKs (rel t ycov <> xcov) z
      Nothing -> pure ()

-- signumi x y means y := signum x
signumi :: MonadRef m => Interval m -> Interval m -> m ()
signumi (Concrete a) b = eqz b (signum a)
signumi a (Concrete 0) = eqz a 0
signumi i j = do
  zle (-1) j *> lez j 1
  onLo j $ \lo -> zle lo i -- check the math, this works
  onHi j $ \hi -> lez i hi
  onLo i $ \lo -> zle (signum lo) j
  onHi i $ \hi -> lez j (signum hi)

neg :: Interval m -> Interval m
neg (Concrete i) = Concrete (negate i)
neg (Interval m c) = Interval (Aff NegativeOne 0 <> m) c

-- negatei x y means y := negate x, x = negate y
negatei :: MonadRef m => Interval m -> Interval m -> m ()
negatei i j = eq i (neg j)

-- absolute x y means y := abs x
absi :: MonadRef m => Interval m -> Interval m -> m ()
absi (Concrete a) i = eqz i (abs a)
absi i (Concrete a) = eqz i a <|> eqz i (negate a) -- case split rather than lose precision
absi i j = do
  zle 0 j -- absolute value is at least 0
  range i >>= \case
    (Nothing,Nothing) -> pure ()
    (Just x, _) | x >= 0 -> eq i j -- strictly positive, unify!
    (_, Just y) | y <= 0 -> negatei i j -- can't unify, but easy to communicate
    (mx,my) -> do
      for_ (liftA2 (\x y -> negate x `max` y) mx my) $ lez j
      zle 1 j *> negatei i j <|> eq i j -- split early

abstract :: Z -> Interval m
abstract = Concrete

known :: MonadRef m => Interval m -> m (Maybe Z)
known i = range i <&> \case
  (Just x, Just y) | x == y -> Just x
  _ -> Nothing

refine :: MonadRef m => Interval m -> m (Maybe Z)
refine (Concrete a) = pure (Just a)
refine i = range i >>= \case
  (Just lo, Just hi)
     | lo == hi -> pure $ Just lo
     | m <- div (lo + hi) 2 -> cut m (lo <$ guard (lo == m)) (hi <$ guard (hi == m+1))
  (Just lo, Nothing)
     | lo >= 0   -> cut (2*lo) (0    <$ guard (lo ==  0)) Nothing
     | otherwise -> cut (-1)   ((-1) <$ guard (lo == -1)) Nothing
  (Nothing, Just hi) -> cut (if hi < 0 then 2*hi else -1) Nothing ((-1) <$ guard (hi == -1))
  (Nothing, Nothing) -> cut (-1) Nothing Nothing
 where cut m p q = p <$ lez i m
               <|> q <$ gtz i m

-- gradually reduce to a single integer, incrementally, letting propagators push information around as we go
-- this refines until it yields an answer
concrete :: MonadSignal e m => Interval m -> m Z
concrete i = refine i >>= \case
  Nothing -> concrete i
  Just z -> pure z

interval :: MonadSignal e m => Maybe Z -> Maybe Z -> m (Interval m)
interval (Just lo) (Just hi)
  | lo == hi = pure $ Concrete lo -- one value, nothing will ever propagate
  | lo > hi  = empty -- empty interval, fail now
interval mlo mhi = do
  let me = maybe (Left Nil) Right
  ref <- newRef (Root $ R 0 (me mlo) (me mhi) nil nil nil) -- build the interval
  -- TODO: topological sort grounding order
  let simple (Left Nil) = True
      simple Right{} = True
      simple _ = False
  let i = Interval mempty ref
  grounding $ fix $ \loop ->
    findRef ref >>= \case
      (_, r, _)
         | Nil <- rlop r, Nil <- rhip r, Nil <- rcov r, simple (rlo r), simple (rhi r) -> pure () -- nobody cares, so don't
         -- TODO: properly report multiplicity of the solution
         | otherwise -> refine i >>= maybe loop (const $ pure ())
  pure i

-- bottom = [-infinity,infinity], a fresh interval
bottom :: MonadSignal e m => m (Interval m)
bottom = interval Nothing Nothing

instance Relative (Interval m) where
  rel n (Concrete m) = Concrete (rel n m)
  rel n (Interval m c) = Interval (n <> m) c

-- less than or equal
le, ge, lt, gt, ne, eq :: MonadRef m => Interval m -> Interval m -> m ()
le i (Concrete b) = lez i b
le (Concrete a) i = zle a i
le i j = do
  onHi j $ \hi -> lez i hi -- TODO: this could remove itself whenever dlo > chi
  onLo i $ \lo -> zle lo j -- TODO: this could remove itself whenever chi < dlo

lt i (Concrete b) = lez i (b-1)
lt (Concrete a) i = zle (a+1) i

-- i need to know if the
--
-- i = p(q(croot))
-- j = n(r(droot))
-- croot = q^-1(p^-1(n(r(droot)
-- croot = t(droot)
lt i@(Interval p c) j@(Interval n d) = do
  (q,_,croot) <- findRef c
  (r,_,droot) <- findRef d
  let t = inv (p <> q) <> n <> r
  case t of
    Aff One k -> guard (croot /= droot || k < 0)
    _ -> pure ()
  onHi j $ \hi -> ltz i hi
  onLo i $ \lo -> zlt lo j

eq (Concrete a) i = eqz i a
eq i (Concrete b) = eqz i b
eq (Interval p x) (Interval q y) = eqRef x (inv p <> q) y

ge = flip le
gt = flip lt

overlaps :: (Maybe Z, Maybe Z) -> (Maybe Z, Maybe Z) -> Bool
overlaps (ml, mh) (ml', mh') = lem ml mh' && lem ml' mh where
  lem (Just x) (Just y) = x <= y
  lem _ _ = True

ne i (Concrete b) = nez i b
ne (Concrete a) i = nez i a
ne i@(Interval p c) j@(Interval n d) = do
  (p',_,croot) <- findRef c
  (n',_,droot) <- findRef d
  let t = inv p' <> inv p <> n <> n'
  case t of
    Aff One k | croot == droot -> guard (k /= 0)
    Aff One _ -> do
      rd <- rangeRef t droot
      rc <- rangeRef mempty croot
      when (overlaps rc rd) $ do
        onceKnown i $ nez j
        onceKnown j $ nez i
    Aff NegativeOne k
      | croot == droot, odd k -> pure () -- forall i ∈ croot. 2i /= d
      | otherwise -> do
        rd <- rangeRef t droot
        rc <- rangeRef mempty croot
        when (overlaps rc rd) $ do
          onceKnown i $ nez j
          onceKnown j $ nez i

-- current range
rangeRef :: MonadRef m => Aff -> C m -> m (Maybe Z, Maybe Z)
rangeRef z c = findRef c <&> \ (cd', R _ mlo mhi _ _ _, _) -> (simp cd' mlo, simp cd' mhi) where
  simp :: Aff -> Either x Integer -> Maybe Integer
  simp i (Right j) = Just (rel (z<>i) j)
  simp _ Left{} = Nothing

range :: MonadRef m => Interval m -> m (Maybe Z, Maybe Z)
range (Concrete a) = pure (Just a, Just a)
range (Interval n c) = rangeRef n c
