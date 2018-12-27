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
  , union
  , interval
  , bottom
  , addz
  , signumi
  , negatei
  , absi
  -- relations
  , lt,  le,  eq,  ne,  ge,  gt
  , zlt, zle, zeq, zne, zge, zgt
  , ltz, lez, eqz, nez, gez, gtz
  , onceBoundedBelow, onceBoundedAbove
  , onceKnown, onHi, onLo
  , known
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor
import Data.Functor
import Data.Foldable
import Data.Monoid
import Ref
import Signal
import Relative.Internal as R

type C m = RefM m (Content m)

-- interval propagators
data P r m = P !Z (Z -> r -> m ())

instance Relative (P r m) where
  rel d (P d' f) = P (d+d') f

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

data Content m
  = Root {-# unpack #-} !(R m) -- we are the root
  | Child !Z {-# unpack #-} !(C m) -- union-find child, with parent pointer and an integer offset from the parent

instance Relative (Content m) where
  rel d (Root r) = Root (rel d r)
  rel d (Child d' z) = Child (d + d') z

-- non-relational intervals, made relational by adding propagators to push information around
-- and working in a non-deterministic setting
data Interval m
  = Concrete !Z
  | Interval !Z {-# unpack #-} !(C m)

findRef :: MonadRef m => C m -> m (Z, R m, C m)
findRef c = readRef c >>= \case
  Root r -> pure (0,r,c)
  Child z c' -> do
    (z', r, root) <- findRef c'
    let !z'' = z + z'
    (z'', r, root) <$ writeRef c (Child z'' root)

-- bound below by a constant
zlt, zle, zeq, zne, zge, zgt :: MonadRef m => Z -> Interval m -> m ()
zle lo (Concrete a) = guard (lo <= a)
zle lo (Interval m c) = do
  (d, R rank olo ohi lop hip cov, croot) <- findRef c
  for_ ohi $ \hi -> guard (lo <= hi) -- check we're not empty
  case olo of
    Left ps -> do
      writeRef croot $ Root $ mkR rank (Right (lo-m-d)) ohi lop hip cov
      runPs (lop <> ps) croot
      case ohi of
        Right x | x == lo-m-d -> runKs cov (lo-m-d)
        _ -> pure ()
      -- run covering
    Right lo2 -> when (lo > lo2) $ do
      writeRef croot $ Root $ mkR rank (Right (lo-m-d)) ohi lop hip cov
      runPs lop croot
      case ohi of
        Right x | x == lo-m-d -> runKs cov (lo-m-d)
        _ -> pure ()

zgt = flip ltz
zge = flip lez
zlt z i = zle (z+1) i
zeq = flip eqz
zne = flip nez


-- bound above by a constant
ltz, lez, gez, gtz :: MonadRef m => Interval m -> Z -> m ()
lez (Concrete a) hi = guard (a <= hi)
lez (Interval m c) hi = do
  (d, R rank olo ohi lop hip cov, croot) <- findRef c
  for_ olo $ \lo -> guard (lo <= hi) -- check we're not empty
  case ohi of
    Left qs -> do
      writeRef croot $ Root $ mkR rank olo (Right (hi-m-d)) lop hip cov
      runPs (hip <> qs) croot
      case olo of
        Right x | x ==hi-m-d -> runKs cov (hi-m-d)
        _ -> pure ()
    Right hi2 -> when (hi < hi2) $ do
      writeRef croot $ Root $ mkR rank olo (Right (hi-m-d)) lop hip cov
      runPs hip croot
      case olo of
        Right x | x ==hi-m-d -> runKs cov (hi-m-d)
        _ -> pure ()


ltz i z = lez i (z-1)
gtz = flip zlt
gez = flip zle

-- eqz i z = do zli z i; lez i z
eqz :: MonadRef m => Interval m -> Z -> m ()
eqz (Concrete a) b = guard (a == b)
eqz (Interval m c) b = do
  (d, R rank olo ohi lop hip cov, croot) <- findRef c
  ps <- case olo of
    Right lo -> case compare lo b of
      LT -> pure lop
      EQ -> pure mempty
      GT -> empty
    Left ps -> pure $ ps <> lop
  qs <- case ohi of
    Right hi -> case compare b hi of
      LT -> pure hip
      EQ -> pure mempty
      GT -> empty
    Left qs -> pure $ qs <> hip
  let rb = Right (b-d-m)
  writeRef croot $ Root $ mkR rank rb rb nil nil nil
  runPs (ps <> qs) croot
  runKs cov (b-d-m)

nez :: MonadRef m => Interval m -> Z -> m ()
nez (Concrete b) z = guard (b /= z)
nez i@Interval{} z = zle (z+1) i <|> lez i (z-1)

onceBoundedBelow :: MonadRef m => Interval m -> m () -> m ()
onceBoundedBelow Concrete{} m = m
onceBoundedBelow (Interval _ c) m = do
  (_, r, croot) <- findRef c
  case rlo r of
    Left ps -> writeRef croot $ Root r { rlo = Left $ ps `R.snoc` P 0 (\_ _ -> m) }
    Right{} -> m -- already bounded below

onceBoundedAbove :: MonadRef m => Interval m -> m () -> m ()
onceBoundedAbove Concrete{} m = m
onceBoundedAbove (Interval _ c) m = do
  (_, r, croot) <- findRef c
  case rhi r of
    Left qs -> writeRef croot $ Root r { rhi = Left $ qs `R.snoc` P 0 (\_ _ -> m) }
    Right{} -> m -- already bounded above

onLo :: MonadRef m => Interval m -> (Z -> Maybe Z -> m ()) -> m () -- every improvement to the lower bound triggers this, including the initial non-bottom change
onLo (Concrete a) f = f a (Just a)
onLo (Interval n c) f = do
  (d, r, croot) <- findRef c
  case covered r of
    Just z -> f (n+z+d) $ Just (n+z+d) -- done, nothing will ever improve this, nothing to watch
    Nothing -> do
      let f' z e = findRef e >>= \ (j, R { rlo = Right k, rhi = u }, _) -> f (z+j+k) $ either (const Nothing) (\l -> Just $ z+j+l) u
      writeRef croot $ Root r { rlop = rlop r `R.snoc` P (n+d) f' }

onHi :: MonadRef m => Interval m -> (Maybe Z -> Z -> m ()) -> m ()
onHi (Concrete a) f = f (Just a) a
onHi (Interval n c) f = do
  (d, r, croot) <- findRef c
  case covered r of
    Just z -> f (Just (n+z+d)) (n+z+d) -- done, nothing will ever improve this, nothing to watch
    Nothing -> do
      let f' z e = findRef e >>= \ (j, R { rlo = u, rhi = Right k }, _) -> f (either (const Nothing) (\l -> Just $ z+j+l) u) (z+j+k)
      writeRef croot $ Root r { rhip = rhip r `R.snoc` P (n+d) f' }

onceKnown :: MonadRef m => Interval m -> (Z -> m ()) -> m ()
onceKnown (Concrete a) f = f a
onceKnown (Interval n c) f = do
  (d, r, croot) <- findRef c
  case covered r of
    Just z -> f (n+z+d)
    Nothing -> writeRef croot $ Root r { rcov = rcov r `R.snoc` P (n+d) (\a b -> f (a+b)) }

maxx, minx :: Ps m -> Ps m -> Z -> Either (Ps m) Z -> Either (Ps m) Z -> (Ps m, Ps m, Either (Ps m) Z)
maxx xap xbp o (Right a) (Right b)  = (if a < b then xap else nil, if b < a then xbp else nil, Right $ max (o + a) b)
maxx _ _   _ (Left ps) (Left ps') = (nil, nil, Left $ ps <> ps')
maxx xap _  _ (Left ps) rb@Right{} = (xap <> ps, nil, rb)
maxx _ xbp  o (Right a) (Left ps)  = (nil, xbp <> ps, Right $ o + a)

minx xap xbp o (Right a) (Right b)  = (if a > b then xap else nil, if b > a then xbp else nil, Right $ min (o + a) b)
minx _ _   _ (Left ps) (Left ps') = (nil, nil, Left $ ps <> ps')
minx xap _  _ (Left ps) rb@Right{} = (xap <> ps, nil, rb)
minx _ xbp  o (Right a) (Left ps)  = (nil, xbp <> ps, Right $ o + a)

ltLo :: Ord c => Either a c -> Either b c -> Bool
ltLo Left{} Left{}  = False
ltLo Left{} Right{} = True
ltLo Right{} Left{} = False
ltLo (Right a) (Right b) = a < b

ltHi :: Ord c => Either a c -> Either b c -> Bool
ltHi Left{} Left{}  = False
ltHi Left{} Right{} = False
ltHi Right{} Left{} = True
ltHi (Right a) (Right b) = a < b

-- this is a special case when we go to define 'addition' and only two of the entries are non-constant
-- and is implemented by union-find rather than propagation
-- union x y d means x := y + d
unionRef :: MonadRef m => C m -> C m -> Z -> m ()
unionRef x y d = do
  (xd, R xrank xlo xhi xlop xhip xcov, xroot) <- findRef x
  (yd, R yrank ylo yhi ylop yhip ycov, yroot) <- findRef y
  if xrank < yrank then do
    writeRef xroot $ Child d yroot
    let offset = xd - d - yd
        (psx,psy,ylo') = maxx xlop ylop offset xlo ylo
        (qsx,qsy,yhi') = minx xhip yhip offset xhi yhi
    for_ ylo' $ \ a -> for_ yhi' $ \b -> guard (a <= b) -- abort on empty intervals
    let result = mkR yrank ylo' yhi' (rel (-d) xlop <> ylop) (rel (-d) xhip <> yhip) (rel (-d) xcov <> ycov)
    writeRef yroot $ Root result
    when (ltLo ylo ylo') $ do
      runPs (ylop<>psy) yroot
      runPs psx xroot
    when (ltHi yhi' yhi) $ do
      runPs (yhip<>qsy) yroot
      runPs qsx xroot
    case covered result of
      Just z -> do runKs ycov z; runKs xcov $ z-offset
      Nothing -> pure ()
  else do
    writeRef yroot $ Child (-d) xroot
    let offset = yd + d - xd
        (psx,psy,xlo') = maxx xlop ylop offset xlo ylo
        (qsx,qsy,xhi') = minx xhip yhip offset xhi yhi
        xrank' = if xrank == yrank then xrank + 1 else xrank
    for_ xlo' $ \a -> for_ xhi' $ \b -> guard (a <= b) -- abort on empty intervals
    let result = mkR xrank' xlo' xhi' (xlop <> rel d ylop) (xhip <> rel d yhip) (xcov <> rel d ycov)
    writeRef xroot $ Root result
    when (ltLo xlo xlo') $ do
      runPs (psx<>xlop) xroot
      runPs psy yroot
    when (ltHi xhi' xhi) $ do
      runPs (qsx<>xhip) xroot
      runPs qsy yroot
    case covered result of
      Just z -> do runKs xcov z; runKs ycov $ z-offset
      Nothing -> pure ()

-- signumi x y means y := signum x
signumi :: MonadRef m => Interval m -> Interval m -> m ()
signumi (Concrete a) b = eqz b (signum a)
signumi a (Concrete 0) = eqz a 0
signumi i j = do
  zle (-1) j *> lez j 1
  onLo j $ \lo _ -> zle lo i -- check the math, this works
  onHi j $ \_ hi -> lez i hi
  onLo i $ \lo _ -> zle (signum lo) j
  onHi i $ \_ hi -> lez j (signum hi)

-- negatei x y means y := negate x, x = negate y
negatei :: MonadRef m => Interval m -> Interval m -> m ()
negatei (Concrete a) b = eqz b (negate a)
negatei a (Concrete b) = eqz a (negate b)
negatei i j = do
  onLo i $ \lo _ -> lez j (negate lo)
  onHi i $ \_ hi -> zle (negate hi) j
  onLo j $ \lo _ -> lez i (negate lo)
  onHi j $ \_ hi -> zle (negate hi) i

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

-- x := y + d
union :: MonadRef m => Interval m -> Interval m -> Z -> m ()
union (Concrete a) i d = eqz i (a - d)
union i (Concrete b) d = eqz i (b + d)
union (Interval p x) (Interval q y) z = unionRef x y (q - p + z)

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
  let i = Interval 0 ref
  grounding $ fix $ \loop ->
    findRef ref >>= \case
      (_, r, _)
         | Nil <- rlop r, Nil <- rhip r, Nil <- rcov r, simple (rlo r), simple (rhi r) -> pure () -- nobody cares, so don't
         -- TODO: properly report multiplicity of the solution
         | otherwise -> refine i >>= maybe loop (const $ pure ())
  pure $ Interval 0 ref

-- bottom = [-infinity,infinity], a fresh interval
bottom :: MonadSignal e m => m (Interval m)
bottom = interval Nothing Nothing

-- add a constant to an interval
addz :: Z -> Interval m -> Interval m
addz n (Concrete m) = Concrete (n + m)
addz n (Interval m c) = Interval (n + m) c

-- less than or equal
le, ge, lt, gt, ne, eq :: MonadRef m => Interval m -> Interval m -> m ()
le i (Concrete b) = lez i b
le (Concrete a) i = zle a i
le i j = do
  onHi j $ \_ hi -> lez i hi -- TODO: this could remove itself whenever dlo > chi
  onLo i $ \lo _ -> zle lo j -- TODO: this could remove itself whenever chi < dlo

lt i (Concrete b) = lez i (b-1)
lt (Concrete a) i = zle (a+1) i
lt i@(Interval p c) j@(Interval n d) = do
  ((+) p -> p',_,croot) <- findRef c
  ((+) n -> n',_,droot) <- findRef d
  guard (croot /= droot || p' < n') -- check that we aren't the same reference, or if we are, that we reference an earlier offset
  onHi j $ \_ hi -> lez i hi
  onLo i $ \lo _ -> zle lo j

eq x y = union x y 0

ge = flip le
gt = flip lt

ne i (Concrete b) = nez i b
ne (Concrete a) i = nez i a
ne i@(Interval p c) j@(Interval n d) = do
  ((+) p -> p',_,croot) <- findRef c
  ((+) n -> n',_,droot) <- findRef d
  guard (croot /= droot || p' /= n') -- check that we _can_ be not equal to the other reference
  onceKnown i $ nez j
  onceKnown j $ nez i

-- current range
rangeRef :: MonadRef m => Z -> C m -> m (Maybe Z, Maybe Z)
rangeRef z c = findRef c <&> \ (cd', R _ mlo mhi _ _ _, _) -> (simp cd' mlo, simp cd' mhi) where
  simp :: Integer -> Either x Integer -> Maybe Integer
  simp i (Right j) = Just (z + i + j)
  simp _ Left{} = Nothing

range :: MonadRef m => Interval m -> m (Maybe Z, Maybe Z)
range (Concrete a) = pure (Just a, Just a)
range (Interval n c) = rangeRef n c

{-
-- from here down is nonsense

-- check if already concrete, but do not force any decisions yet
tryConcrete :: MonadSignal e m => C m -> m (Either (Z, R m, C m) Natural)
tryConcrete c = go <$> findEx c where
  go (d, R _ (Just lo) (Just hi) _ _, croot) | lo == hi = Right (d + lo)
  go l = Left l


-- add a list of positive values to a list of negative values in such a way that the total equals 0.
addsub :: [Interval m] -> [Interval m] -> m ()
addsub xs ys = do
  let tally (t,xs) (Concrete a) = pure (t + a, xs)
      tally (t,xs) (Interval m c) = do
        (d, _, root) <- findRef c
        pure (t + m + d, root:xs)
  (p,ps) <- foldM tally (0,[]) xs
  (n,qs) <- foldM tally (0,[]) ys
  let pn = p-n
  let zs = (False,)<$>ps <> (True,)<$>qs
  case zs of
    [] -> guard (p == n) -- concrete, guard
    [(b,i)] -> eqz i (if b then n-p else p-n) -- one interval, force it, if b then  p-n = -i else p-n = i
    [(a,i),(b,j)] | a == not b -> unionRef i j (if b then n-p else p-n) -- if b then i = j + n - p
    bx0:bx1:zs0 -> do
      w1 <- newRef zs0
      w2 <- newRef zs0
      let advance b r k = do
            mhead <- updateRef r $ \case
              [] -> (Nothing, [])
              x:xs -> (Just x, xs)
            case mhead of
              Nothing -> k
              Just (u,c) -> step2 b u c r k
          step2 b u c r k = do
            (d, R rank mlo mhi lop hip, croot) <- findRef c
            if xor b u then case mlo of
               Left ps -> writeRoot croot $ Root $ R rank (Left (R.snoc ps $ P 0 $ \_ -> advance b r k) mhi lop hip
               Right _ -> advance b r k
            else case mhi of
               Left qs -> writeRoot croot $ Root $ R rank mlo (Left (R.snoc qs $ P 0 $ \_ -> advance b r k) lop hip
               Right _ -> advance b r k
          watch2 b (b0,x0) (b1,x1) r k = do
            step2 b b0 x0 r k
            step2 b b1 x1 r k


      watch2 False bx0 bx1 w1 (propLo zs)
      watch2 True  bx0 bx1 w2 (propHi zs)
      -- we need to establish two 2-watched literal schemes to propagate bounds

  if null zs then guard (p == n)
  else case zs of


  let watchLo1 d [] k = k d
      watchLo1 d (r:rs) k = do
        (d', R rank mlo mhi mlop mhip, root) <- findRef r
        case mlo of
          Left ps -> writeRef root $ Root $ R rank (mlo `R.snoc` stepLo k (d+d')) mhi mlop mhip -- block waiting with 2-watched literal

          Right d'' -> watchLo (d+d'+d'') rs k -- keep going
  let watchHi1 d [] k = k d
      watchHi1 d (r:rs) k = do
        (d', R rank mlo mhi mlop mhip, root) <- findRef r
        case mhi of
          Left qs -> writeRef root $ Root $ R rank mlo (mhi `R.snoc` stepHi k (d+d')) mlop mhip -- block waiting with 2-watched literal
          Right d'' -> watchLo (d+d'+d'') rs k -- keep going

  watchLo1 p ps $ \p' -> watchHi1 n qs $ \q' -> watchAll (p' - q') ps qs
  watchHi1 p ps $ \p' -> watchLo1 n qs $ \q' -> watchAll (q' - p') qs ps
  -- scan through rs looking for one that
  -- add a 1-watch literal scheme for slow wakeup

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
        result@(Interval z e) <- addz (n + m + cd + dd) <$> interval (liftA2 (+) clo dlo) (liftA2 (+) chi dhi)
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

-}
