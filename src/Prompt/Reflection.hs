{-# language DeriveAnyClass #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Prompt.Reflection
  ( CC
  , runCC
  , runCC_
  ) where

import Aligned.Base
import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Monad as Monad
import Control.Monad.Fail as MonadFail
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Type.Equality
import Prelude hiding (id,(.))
import Prompt.Class
import Ref.Key
import qualified Unaligned.Base as U

type P m = Key (KeyState m) -- prompts
type KC m = Rev Cat (Kleisli (CC m))
type DC m = Rev Cat (Del m)

data Del m a b = Del {-# unpack #-} !(P m b) !(KC m a b)

data SC m a b where
  SC :: !(DC m a w) -> !(KC m w b) -> SC m a b

data CC m a where
  WithSC :: {-# unpack #-} !(P m x) -> (SC m w x -> CC m x) -> !(DC m w y) -> !(KC m y a) -> CC m a
  CC :: m w -> !(DC m w y) -> !(KC m y a) -> CC m a

mapSC :: (forall w. SC m w a -> SC m w b) -> CC m a -> CC m b
mapSC f (WithSC cp ck sd sk) = case f (SC sd sk) of
  SC sd' sk' -> WithSC cp ck sd' sk'
mapSC f (CC p sd sk) = case f (SC sd sk) of
  SC sd' sk' -> CC p sd' sk'
{-# inline mapSC #-}

instance Category (SC m) where
  id = SC id id
  SC cat_bw kwc . SC cat_aw1 kw1b = case unsnoc cat_bw of
    Empty -> SC cat_aw1 (kwc . kw1b)
    t :&: Del p h -> SC (snoc t (Del p (h . kw1b)) . cat_aw1) kwc
  {-# inline (.) #-}

(!>>=) :: CC m a -> SC m a b -> CC m b
m !>>= f = mapSC (f.) m
{-# inline (!>>=) #-}

instance Monad m => Functor (CC m) where
  fmap = liftM
  {-# inlineable fmap #-}

instance Monad m => Applicative (CC m) where
  pure a = CC (pure a) id id
  {-# inlineable pure #-}
  (<*>) = ap
  {-# inlineable (<*>) #-}

instance Monad m => Monad (CC m) where
  WithSC cp ck sd sk >>= f = WithSC cp ck sd (cons (Kleisli f) sk)
  CC p sd sk >>= f = CC p sd (cons (Kleisli f) sk)
  {-# inlineable (>>=) #-}

  fail = lift . Monad.fail
 
instance MonadFail m => MonadFail (CC m) where
  fail = lift . MonadFail.fail

instance MonadIO m => MonadIO (CC m) where
  liftIO = lift . liftIO
  {-# inlineable liftIO #-}

instance PrimMonad m => PrimMonad (CC m) where
  type PrimState (CC m) = PrimState m
  primitive f = lift (primitive f)
  {-# inlineable primitive #-}

instance MonadKey m => MonadKey (CC m) where
  type KeyState (CC m) = KeyState m

instance MonadTrans CC where
  lift m = CC m id id
  {-# inlineable lift #-}

instance MonadKey m => MonadPrompt (CC m) where
  type Prompt (CC m) = P m
  type Sub (CC m) = SC m
  newPrompt = CC newKey id id
  {-# inlineable newPrompt #-}
  pushPrompt p = mapSC $ \(SC d t) -> SC (cons (Del p t) d) id
  {-# inlineable pushPrompt #-}
  withSub p f = WithSC p f id id
  {-# inlineable withSub #-}
  pushSub = flip (!>>=)
  {-# inlineable pushSub #-}

instance MonadState s m => MonadState s (CC m) where
  get = lift get
  {-# inline get #-}
  put = lift . put
  {-# inline put #-}
  state = lift . state
  {-# inline state #-}

runCC :: forall m a. Monad m => m a -> CC m a -> m a
runCC e = goCC where
  goCC :: CC m a -> m a
  goCC (CC m d l) = m >>= goSC d l
  goCC (WithSC p f d l) = case split p d of
    sc U.:&: r -> goCC $ f sc !>>= SC r l
    U.Empty -> e

  goSC :: Monad m => DC m w x -> KC m x a -> w -> m a
  goSC d l x = case unsnoc d of
    Empty -> case unsnoc l of
      Empty -> pure x
      t :&: Kleisli f -> goCC $! case f x of
        WithSC cp ck sd sk -> WithSC cp ck sd (t . sk)
        CC p sd sk -> CC p sd (t . sk)
    t :&: Del p h -> case unsnoc h of
      Empty -> goSC t l x
      ti :&: Kleisli f -> goCC $ f x !>>= SC (t `snoc` Del p ti) l
{-# inlineable runCC #-}

split :: Key (KeyState m) w -> DC m a b -> U.View (SC m a w) (DC m w b)
split p q = case unsnoc q of
  Empty -> U.Empty
  t :&: Del p' sk -> case testEquality p p' of
    Just Refl -> SC id sk U.:&: t
    Nothing -> case split p t of
      SC dl tl U.:&: sk' -> SC (dl `snoc` Del p' sk) tl U.:&: sk'
      U.Empty -> U.Empty

runCC_ :: MonadFail m => CC m a -> m a
runCC_ = runCC $ MonadFail.fail "missing prompt"
{-# inlineable runCC_ #-}
