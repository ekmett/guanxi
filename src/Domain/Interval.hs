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

{-
maxx, minx :: Ps m -> Ps m -> Aff -> Either (Ps m) Z -> Either (Ps m) Z -> (Ps m, Ps m, Either (Ps m) Z)
maxx xap xbp o (Right a) (Right b) =
  ( if rel o a < b then rel o xap else nil
  , if b < rel o a then xbp else nil
  , Right $ max (rel o a) b)
maxx _ _   o (Left ps) (Left ps') = (nil, nil, Left $ rel o ps <> ps')
maxx xap _ o (Left ps) rb@Right{} = (rel o (xap <> ps), nil, rb)
maxx _ xbp o (Right a) (Left ps)  = (nil, xbp <> ps, Right $ rel o a)

minx xap xbp o (Right a) (Right b) =
  ( if rel o a > b then rel o xap else nil
  , if b > rel o a then xbp else nil
  , Right $ min (rel o a) b
  )
minx _ _    o (Left ps) (Left ps') = (nil, nil, Left $ rel o ps <> ps')
minx xap _  o (Left ps) rb@Right{} = (rel o (xap <> ps), nil, rb)
minx _ xbp  o (Right a) (Left ps)  = (nil, xbp <> ps, Right $ rel o a)

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
-}

-- this is a special case when we go to define 'addition' and only two of the entries are non-constant
-- and is implemented by union-find rather than propagation
-- union x f y means x := f(y), y = (f^-1)(x)
unionRef :: MonadRef m => C m -> Aff -> C m -> m ()
unionRef = undefined
{-
unionRef x d y = do
  (xd, R xrank xlo xhi xlop xhip xcov, xroot) <- findRef x
  (yd, R yrank ylo yhi ylop yhip ycov, yroot) <- findRef y
  if xrank < yrank then do
    writeRef xroot $ Child d yroot
    
    -- let offset = xd <> inv d <> inv yd -- convert yroot into xroot terms
    let offset = yd <> inv d <> inv xd -- convert xroot into yroot terms
    -- yroot = rel offset xroot
    ((psx,psy,ylo'),(qsx,qsy,yhi')) <- case offset of
      Aff One _ -> (maxx xlop ylop offset xlo ylo, minx xhip yhip offset xhi yhi)
      _         -> undefined -- ouch my brain
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
      Just z -> runKs (ycov <> rel (inv offset) xcov) z
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
-}

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
negatei i j = union i (Aff NegativeOne 0) j

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
union :: MonadRef m => Interval m -> Aff -> Interval m -> m ()
union (Concrete a) d i = eqz i (rel (inv d) a)
union i d (Concrete b) = eqz i (rel d b)
union (Interval p x) z (Interval q y) = unionRef x (inv p <> z <> q) y

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

eq x y = union x mempty y
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

{-
-- polynomial inequality \Sum_i a_i x_i <= 0
polyle :: MonadRef m => [(Z, Interval m)] -> m ()
polyle xs = do
  let tally r (0, _) = pure r
      tally (t,ys,n) (k, Concrete a) = pure (t + k*a, ys,n)
      tally (t,ys,n) (k, Interval m c) = do
        (d, _, root) <- findRef c
        pure (t + k*(m+d), (k,root):ys,n+1)
  (p,ps,n) <- foldM tally (0,[]) xs
  
  case ps of
    [] -> guard (p <= 0)
    -- k*c + p <= 0, k /= 0
    [(k,c)] -> (if k > 0 then lez else gez) (Interval 0 c) (negate p `div` k)
    -- 2 or more things, we need to watch until one is left
    (k1,c1):(k2,c2):ps' -> do
        rn <- newRef (n,ps')
        let bound k c = do
              (mlo,mhi) <- rangeRef 0 c
              mb <- if k <0 then (k*)<$> mlo else (k*)<$> mhi
            onceBoundedBy k
              | k < 0     = onceBoundedBelow
              | otherwise = onceBoundedAbove
        -- data Watch a = Zero | One a | Many a
        let watch k c = onceBoundedBy k $ 
              (r,n') <- updateRef rn $ \(n',(p:ps')) -> ((p,n'-1), (n'-1,tail ps')
              
              case n' of
                1 -> -- we're down to one ref, learn its bound first
                0 -> -- we're down to zero refs, 
                     -- everybody has a bound
                     -- will be tedious to Chebyshev fit / compute a Gompertz cut
        watch k1 c1
        watch k2 c2
    
      -- go n ps where
      
  if | [] <- n == 0 -> guard (p <= 0)
     | n == 1 -> lez (Interval 0 (head ps)
  pure ()
-}

{-
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

-}
