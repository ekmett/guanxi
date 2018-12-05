{-# language DeriveAnyClass #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Prompt.Reflection
  ( CC
  , runCC
  , runCC_ -- falls back on MonadFail on missing prompt
  ) where

import Aligned.Base
import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Monad (ap)
import Control.Monad.Fail as MonadFail
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Default
import Data.Functor ((<&>))
import Data.Type.Equality
import Prelude hiding (id,(.))
import Prompt.Class
import Ref.Key

type K m = Rev Cat (Kleisli m)

data Del m a b = Del {-# unpack #-} !(Key (KeyState m) b) !(K (CC m) a b)

type Dels m = Rev Cat (Del m)

data SC m a b where
  SC :: !(Dels m a w) -> !(K (CC m) w b) -> SC m a b

data CC m a where
  WithSC
    :: {-# unpack #-} !(Key (KeyState m) x)
    -> (SC m w x -> CC m x)
    -> {-# unpack #-} !(SC m w a) -> CC m a
  CC :: m w -> {-# unpack #-} !(SC m w a) -> CC m a

mapSC :: (forall w. SC m w a -> SC m w b) -> CC m a -> CC m b
mapSC f (WithSC cp ck s) = WithSC cp ck (f s)
mapSC f (CC p s) = CC p (f s)
{-# inline mapSC #-}

instance Category (SC m) where
  id = SC id id
  SC cat_bw kwc . SC cat_aw1 kw1b = case unsnoc cat_bw of
    Empty -> SC cat_aw1 (kwc . kw1b)
    t :&: Del p h -> SC (snoc t (Del p (h . kw1b)) . cat_aw1) kwc

(!>>=) :: CC m a -> SC m a b -> CC m b
m !>>= f = mapSC (f.) m

instance Monad m => Functor (CC m) where
  fmap f m = m !>>= SC def (singleton (Kleisli (pure . f)))

instance Monad m => Applicative (CC m) where
  pure a = CC (pure a) id
  (<*>) = ap

instance Monad m => Monad (CC m) where
  m >>= f = m !>>= SC def (singleton (Kleisli f))

instance MonadIO m => MonadIO (CC m) where
  liftIO = lift . liftIO

instance PrimMonad m => PrimMonad (CC m) where
  type PrimState (CC m) = PrimState m
  primitive f = lift (primitive f)

instance MonadKey m => MonadKey (CC m) where
  type KeyState (CC m) = KeyState m

instance MonadTrans CC where
  lift m = CC m id

instance MonadKey m => MonadPrompt (CC m) where
  type Prompt (CC m) = Key (KeyState m)
  type Sub (CC m) = SC m
  newPrompt = CC newKey id
  pushPrompt p = mapSC $ \(SC d t) -> SC (cons (Del p t) d) id
  withSub p f = WithSC p f id
  pushSub = flip (!>>=)

instance MonadState s m => MonadState s (CC m) where
  get = lift get
  put = lift . put
  state = lift . state

runCC :: Monad m => m a -> CC m a -> m a
runCC err (CC m r) = m >>= runSC err r
runCC err (WithSC p f (SC d l)) = case split p d of
  Just (sc, r) -> runCC err $ f sc !>>= SC r l
  Nothing -> err

runCC_ :: MonadFail m => CC m a -> m a
runCC_ = runCC $ MonadFail.fail "missing prompt"

runSC :: Monad m => m a -> SC m w a -> w -> m a
runSC err (SC d l) x = case unsnoc d of
  Empty -> case unsnoc l of
    Empty -> pure x
    t :&: Kleisli f -> runCC err $ f x !>>= SC id t
  t :&: Del p h -> case unsnoc h of
    Empty -> runSC err (SC t l) x
    ti :&: Kleisli f -> runCC err $ f x !>>= SC (t `snoc` Del p ti) l

split :: Key (KeyState m) w -> Dels m a b -> Maybe (SC m a w, Dels m w b)
split p q = case unsnoc q of
  Empty -> Nothing
  t :&: Del p' sk -> case testEquality p p' of
    Just Refl -> Just (SC id sk, t)
    Nothing -> split p t <&> \ (SC dl tl, sk') ->
      (SC (dl `snoc` Del p' sk) tl, sk')
