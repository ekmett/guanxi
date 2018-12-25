{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiWayIf #-}
{-# language ViewPatterns #-}

module Domain.Interval
  ( Interval, Z
  , abstract, concrete
  , refine
  , union
  -- relations
  , lt,  le,  eq,  ne,  ge,  gt
  , ltz, lez, eqz, nez, gez, gtz
  , zlt, zle, zeq, zne, zge, zgt
  -- 
  -- TODO:
  -- arbitrary addition/subtraction of intervals
  -- linear equations via propagators
  -- propagation hooks
  , onceBoundedBelow, onceBoundedAbove
  , onceKnown, onHi, onLo
  , where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Numeric.Natural
import Ref
import Signal
import Relative.Base as R

type C m = RefM m (Content m)

-- interval propagators
data P r m = P {-# unpack #-} !Z (Z -> r -> m ())

instance Relative (P r m) where
  rel d (P d' f) = P (d+d') f

runP :: P m -> r -> m ()
runP (P d f) c = f d c

-- TODO: use a map from a unique to an interval propagator 
-- this means giving us disjoint union with stable names?
-- or baking the position info into the propagator so it can delete itself?
-- this implies all the hell of a typical subscription service
type Ps m = R.Cat (P (C m) m) 
type Ks m = R.Cat (P Z m)

runPs, runKs :: R.Cat (P r m) -> r -> m ()
runPs ps root = getAp $ foldMapCat (\p -> Ap $ runP p root) ps
runKs = runPs 

type Z = Integer

data R m = R
  { rrank :: {-# unpack #-} !Int -- rank for union-find
  , rlo   :: !(Either (Ps m) Z) -- lo -- either something waiting as a one-shot for this to become something other than negative infinity or a value
  , rhi   :: !(Either (Ps m) Z) -- hi -- either something waiting as a one-shot for this to become something other than positive infinity or a value
  , rlop  :: !(Ps m) -- lo propagators -- these are removed after we go down to an interval of size one -- TODO: allow these to turn themselves off earlier
  , rhip  :: !(Ps m) -- hi propagators -- these are removed after we go down to an interval of size one -- TODO: allow these to turn themselves off earlier
  , rcov  :: !(Ks m) -- once fully known propagators
  }

-- intelligently strip propagators if covered
mkR :: Int -> Either (Ps m) Z -> Either (Ps m) Z -> Ps m -> Ps m -> Ks m -> R m
mkR r (Right x) (Right y) _ _ _ | x == y = R r (Right x) (Right y) nil nil nil
mkR r mlo mhi lop hip cov = R r mlo mhi lop hip cov

-- is this concrete?
covered :: R m -> Maybe Z
covered (R _ (Right x) (Right y) _ _ _) | x == y = Just x
covered _ = Nothing

pattern Covered :: Z -> R m 
pattern Covered z <- (covered -> Just z) where
  Covered z = R 0 (Right z) (Right z) nil nil nil

data Content m
  = Root {-# unpack #-} !(R m) -- we are the root
  | Child !Z {-# unpack #-} !(C m) -- union-find child, with parent pointer and an integer offset from the parent

instance Relative (Content m) where
  rel d (Root rank lo hi lop hip cov) = Root rank (rel d lo) (rel d hi) (rel d lop) (rel d hip) (rel d cov)
  rel d (Child d' z) = Child (d + d') z

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
zle, zge :: MonadRef m => Z -> Interval m -> m ()
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
      
zge = flip lez

-- bound above by a constant
lez, gez :: MonadRef m => Interval m -> Z -> m ()
lez (Concrete a) hi = guard (a <= hi)
lez (Interval m c) hi = do
  (d, R rank olo ohi lop hip, croot) <- findRef c
  for olo $ \lo -> guard (lo <= hi) -- check we're not empty
  case ohi of
    Left qs -> do
      writeRef croot $ Root $ mkR rank olo (Right (hi-m-d)) lop hip
      runPs (hip <> qs) croot
      case olo of
        Right x | x ==hi-m-d -> runKs cov (hi-m-d)
        _ -> pure ()
    Right hi2 -> when (hi < hi2) $ do
      writeRef croot $ Root $ mkR rank olo (Right (hi-m-d)) lop hip
      runPs hip croot
      case olo of
        Right x | x ==hi-m-d -> runKs cov (hi-m-d)
        _ -> pure ()

gez = flip zle

-- eqz i z = do zli z i; lez i z
eqz :: MonadRef m => Interval m -> Z -> m ()
eqz (Concrete a) b = guard (a == b)
eqz (Interval m c) b = do
  (d, R rank olo ohi lop hip, croot) <- findRef c
  ps <- case olo of
    Right lo -> case compare lo b of
      LT -> pure lop
      EQ -> pure nil
      GT -> empty
    Left ps -> pure $ ps ++ lop
  qs <- case ohi of
    Right hi -> case compare b hi of
      LT -> pure hip
      EQ -> pure nil
      GT -> empty
    Left qs -> pure $ qs ++ hip
  let rb = Right (b - d - m)
  writeRef croot $ Root $ R rank rb rb nil nil
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
    Left ps -> writeRef croot $ Root r { rlo = Left $ ps `R.snoc` P 0 (\_ -> m) }
    Right{} -> m -- already bounded below

onceBoundedAbove :: MonadRef m => Interval m -> m () -> m ()
onceBoundedAbove Concrete{} m = m 
onceBoundedAbove (Interval _ c) m = do
  (_, r, croot) <- findRef c 
  case rhi r of
    Left qs -> writeRef croot $ Root r { rhi = Left $ qs `R.snoc` P 0 (\_ -> m) }
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
onHi (Concrete a) f = f a
onHi (Interval n c) f = do
  (d, r, croot) <- findRef c 
  case covered r of
    Just z -> f (n+z+d) -- done, nothing will ever improve this, nothing to watch
    Nothing -> do
      let f' z e = findRef e >>= \ (j, R { rlo = u, rhi = Right k }, _) -> f (z+j+k) $ either (const Nothing) (\l -> Just $ z+j+l) u
      writeRef croot $ Root r { rhip = rhip r `R.snoc` P (n+d) f' }

onceKnown :: MonadRef m => Interval m -> (Z -> m ()) -> m ()
onceKnown (Concrete a) f = f a
onceKnown (INterval n c) f = do
  (d, r, croot) <- findRef c
  case covered r of
    Just z -> f (n+z+d)
    Nothing -> writeRef croot $ Root r { rcov = rcov r `R.snoc` P (n+d) (\a b -> f (a+b)) }
  
maxx, minx :: Ps m -> Ps m -> Z -> Either (Ps m) Z -> Either (Ps m) Z -> (Ps m, Ps m, Either (Ps m) Z)
maxx ap bp o (Right a) (Right b)  = (if a < b then ap else nil, if b < a then bp else nil, Right $ max (o + a) b)
maxx _ _   _ (Left ps) (Left ps') = (nil, nil, Left $ ps ++ ps')
maxx ap _  _ (Left ps) rb@Right{} = (ap <> ps, nil, rb)
maxx _ bp  o (Right a) (Left ps)  = (nil, bp <> ps, Right $ o + a)

minx ap bp o (Right a) (Right b)  = (if a > b then ap else nil, if b > a then bp else nil, Right $ min (o + a) b)
minx _ _   _ (Left ps) (Left ps') = (nil, nil, Left $ ps ++ ps')
minx ap _  _ (Left ps) rb@Right{} = (ap <> ps, nil, rb)
minx _ bp  o (Right a) (Left ps)  = (nil, bp <> ps, Right $ o + a)

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
    let result = mkR yrank ylo' yhi' (rel (-d) xlop <> ylop) (rel (-d) xhip <> yhip)
    writeRef yroot $ Root result
    when (ylo' > ylo) $ runP ylop yroot
    when (yhi' < yhi) $ runP yhip yroot
    case covered result of
      Just z -> do runKs ycov z; runKs xcov $ z-offset
      Nothing -> pure ()
  else do
    writeRef yroot $ Child (-d) xroot
    let offset = yd + d - xd
        (psx,psy,ylo') = maxx xlop ylop offset xlo ylo
        (qsx,qsy,yhi') = minx xhip yhip offset xhi yhi
        xrank' = if xrank == yrank then xrank + 1 else xrank
    for xlo' $ \a -> for_ xhi' $ \b -> guard (a <= b) -- abort on empty intervals
    let result = mkR xrank' xlo' xhi' (xlop <> rel d ylop) (xhip <> rel d yhip)
    writeRef xroot $ Root result
    when (xlo' > xlo) $ runP xlop xroot
    when (xhi' < xhi) $ runP xhip xroot
    case covered result of
      Just z -> do runKs xcov z; runKs ycov $ z-offset
      Nothing -> pure ()

-- x := y + d
union :: MonadRef m => Interval m -> Interval m -> Z -> m ()
union (Concrete a) i d = eqz i (a - d)
union i (Concrete b) d = eqz i (b + d)
union (Interval p x) (Interval q y) z = unionRef x y (q - p + z)

-- take one step refining an interval into smaller intervals
-- this produces at least one closed interval at each step
-- those intervals get larger and larger 
refineRef :: MonadRef m => C m -> m (Maybe Z)
refineRef c = do
  (d, R rank mlo mhi lop hip cov, root) <- findRef c
  let setLo x = do
        let r' = r { rlo = Right x }
        r' <$ writeRef root $ Root r'
      setHi x = do
        let r' = r { rhi = Right x } 
        r' <$ writeRef root $ Root r'
  mz <- fmap covered $ if
    | Just lo <- mlo, Just hi <- mhi, m <- div (lo + hi) 2 -- we have a finite interval, split it
      -> if lo == hi then pure (Just lo) -- single value, can't refine
         else do let pm = m-1
                 guard (pm >= lo)
                 result <- setHi pm
                 result <$ when (pm < hi) (runP hip root)
          <|> do let progress = m > lo
                 guard (m <= hi)
                 result <- setLo m
                 result <$ when (m > lo) (runP lop root)
    | Just lo <- mlo, lo >=0, Nothing <- mhi, mid <- 2*lo -- non-negative, split using doubling open ended search scheme
        -> do result <- setHi mid
              result <$ runP hip root -- we went from an infinite upper bound to a finite upper bound, definite progress
       <|> do result <- setLo (mid+1)
              result <$ runP lop root -- lo >= 0 ==> 2*lo+1 > lo, so we made progress
    | Nothing <- mlo, Just hi <- mhi, hi < 0, mid <- 2*hi
        -> do result <- setLo (mid+1)
              result <$ runP lop root
       <|> do result <- setHi mid
              result <$ runP hip root
    | otherwise -- mixed negative and positive, split arbitrarily at 0, TODO check for finiteness first
        -> do result <- setHi (-1)
              result <$ runP hip root
       <|> do result <- setLo 0
              result <$ runP lop root
  -- cleanup
  case mz of
    Just z -> (z+d) <$ runKs cov z -- coverage cleanup
    Nothing -> pure Nothing

abstract :: Z -> Interval m
abstract = Concrete

-- perform one step of refining an interval towards concrete values
refine :: MonadSignal e m => Interval m -> m (Maybe Z)
refine (Concrete a) = pure (Just a)
refine (Interval d r) = fmap (d+) <$> refineRef r

-- gradually reduce to a single integer, incrementally, letting propagators push information around as we go
-- this refines until it yields an answer
concrete :: MonadSignal e m => Interval m -> m Z
concrete (Concrete a) = pure a
concrete (Interval d r) = loop where
  loop = refineRef r >>= \case
    Nothing -> loop
    Just z -> pure (d+z)

interval :: MonadSignal e m => Maybe Z -> Maybe Z -> m (Interval m)
interval (Just lo) (Just hi)
  | lo == hi = pure $ Concrete lo -- one value, nothing will ever propagate
  | lo > hi  = empty -- empty interval, fail now
interval mlo mhi = do
  ref <- newRef (Root $ R 0 mlo mhi nil nil) -- build the interval
  -- TODO: topological sort grounding order
  let simple (Left Nil) = True
      simple Right{} = True
      simple _ = False
  grounding $ findRef ref >>= \case
    (_, r, _) | Nil <- rlop r, Nil <- rhip r, Nil <- rcov r, simple (rlo r), simple (rhi r) -> pure ()
      -- nobody cares if we ground this, so don't
      -- TODO: report the multiplicity of the current solution by multiplying it by the interval width
    (_, _, root) -> fix $ \loop -> refineRef root >>= maybe loop (const $ pure ())
  pure $ Interval 0 ref

-- bottom = [-infinity,infinity], a fresh interval
bottom :: MonadSignal e m => m (Interval m)
bottom = interval Nothing Nothing

-- add a constant to an interval
addZ :: Z -> Interval m -> Interval m
addZ n (Concrete m) = Concrete (n + m)
addZ n (Interval m c) = Interval (n + m) c

-- less than or equal
le, ge, lt, gt, ne, eq :: MonadRef m => Interval m -> Interval m -> m ()
le i (Concrete b) = lez i b
le (Concrete a) i = zle a i
le i@(Interval p c) j@(Interval n d) = do
  onHi d $ \_ hi -> lez i (hi+n) -- TODO: this could remove itself whenever dlo > chi 
  onLo c $ \lo _ -> zle (lo+p) j -- TODO: this could remove itself whenever chi < dlo

lt i (Concrete b) = lez i (b-1)
lt (Concrete a i) = zle (a+1) i
lt i@(Interval p c) j@(Interval n d) = do
  ((+) p -> p',_,croot) <- findRef c
  ((+) n -> n',_,droot) <- findRef d
  guard (croot /= droot || p' < n') -- check that we aren't the same reference, or if we are, that we reference an earlier offset
  onHi droot $ \_ hi -> lez i (hi+n'-1)
  onLo croot $ \lo _ -> zle (lo+p'+1) j

eq x y = union x y 0

ge = flip le
gt = flip lt

ne :: MonadRef m => Interval m -> Interval m -> m ()
ne i (Concrete b) = nez i b
ne (Concrete a) i = nez i a 
ne i@(Interval p c) j@(Interval n d) = do
  ((+) p -> p',_ croot) <- findRef c
  ((+) n -> n',_,droot) <- findRef d
  guard (croot /= droot || p' /= n') -- check that we _can_ be not equal to the other reference
  onceKnown croot $ \z -> nez j (z+p')
  onceKnown droot $ \z -> nez i (z+n')

{-
-- from here down is nonsense

-- check if already concrete, but do not force any decisions yet
tryConcrete :: MonadSignal e m => C m -> m (Either (Z, R m, C m) Natural)
tryConcrete c = go <$> findEx c where
  go (d, R _ (Just lo) (Just hi) _ _, croot) | lo == hi = Right (d + lo)
  go l = Left l

-- current range
range :: MonadSignal e m => C m -> m (Maybe Z, Maybe Z)
range c = findRef c <&> \ (cd', R _ mlo mhi _ _, _) -> (fmap (+cd') mlo, fmap (+cd') mhi)

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
  let zs = (False,)<$>ps ++ (True,)<$>qs
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

-}
