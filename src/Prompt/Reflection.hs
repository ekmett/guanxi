{-# language DeriveAnyClass #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language LambdaCase #-}

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

-- Thoughts: if we always used prompts in ascending order, which I intend to, then we could use a
-- type aligned fingertree, rather than Rev Cat in DC, giving slower appends, but giving log
-- time indexing for splitting the continuation. This would require reintroducing the 'P' machinery
-- for producing prompts with an Ord instance, rather than just offering equatable prompts.
--
-- An even fancier version would do something like the Log machinery, but type aligned. There
-- we could delete knowably unnecessary prompts retroactively.

type P m = Key (KeyState m) -- prompts
type KC m = Rev Cat (Kleisli (CC m))
type DC m = Rev Cat (Del m)

data Del m a b = Del {-# unpack #-} !(P m b) !(KC m a b)

data SC m a b where
  SC :: !(KC m w b) -> !(DC m a w) -> SC m a b

data CC m a where
  WithSC :: !(KC m y a) -> !(DC m w y) -> {-# unpack #-} !(P m x) -> (SC m w x -> CC m x) -> CC m a
  CC :: !(KC m y a) -> !(DC m w y) -> m w -> CC m a

instance Category (SC m) where
  id = SC id id
  SC fk fd . SC sk sd = case unsnoc fd of
    Empty -> SC (fk . sk) sd
    t :&: Del p h -> SC fk (snoc t (Del p (h . sk)) . sd)
  {-# inline (.) #-}

bind :: KC m b c -> DC m a b -> CC m a -> CC m c
bind fk fd = case unsnoc fd of
  Empty -> \case
    WithSC sk sd cp ck -> WithSC (fk . sk) sd cp ck
    CC sk sd cc        -> CC (fk . sk) sd cc
  t :&: Del p h -> \case
    WithSC sk sd cp ck -> WithSC fk (snoc t (Del p (h . sk)) . sd) cp ck
    CC sk sd cc        -> CC fk (snoc t (Del p (h . sk)) . sd) cc

instance Applicative m => Functor (CC m) where
  fmap = liftM 
  {-# inlineable fmap #-}

instance Applicative m => Applicative (CC m) where
  pure = CC id id . pure
  {-# inlineable pure #-}
  (<*>) = ap
  {-# inlineable (<*>) #-}

instance Applicative m => Monad (CC m) where
  WithSC sk sd cp ck >>= f = WithSC (cons (Kleisli f) sk) sd cp ck
  CC sk sd p         >>= f = CC     (cons (Kleisli f) sk) sd p
  {-# inlineable (>>=) #-}
  -- fail = CC id id . Monad.fail
 
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
  lift = CC id id
  {-# inlineable lift #-}

instance MonadKey m => MonadPrompt (CC m) where
  type Prompt (CC m) = P m
  type Sub (CC m) = SC m

  newPrompt = lift newKey
  {-# inlineable newPrompt #-}

  pushPrompt p (WithSC sk sd cp ck) = WithSC id (cons (Del p sk) sd) cp ck
  pushPrompt p (CC     sk sd cc)    = CC     id (cons (Del p sk) sd) cc
  {-# inlineable pushPrompt #-}

  withSub = WithSC id id
  {-# inlineable withSub #-}

  pushSub (SC k d) = bind k d
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
  goCC (CC l d m) = m >>= goSC l d
  goCC (WithSC l d p f) = case split p d of
    r U.:&: sc -> goCC $ bind l r (f sc)
    U.Empty -> e

  goSC :: Monad m => KC m x a -> DC m w x -> w -> m a
  goSC l d x = case unsnoc d of
    Empty -> case unsnoc l of
      Empty -> pure x
      t :&: Kleisli f -> goCC $! case f x of
        WithSC sk sd cp ck -> WithSC (t . sk) sd cp ck
        CC sk sd p -> CC (t . sk) sd p
    t :&: Del p h -> case unsnoc h of
      Empty -> goSC l t x
      ti :&: Kleisli f -> goCC $ bind l (t `snoc` Del p ti) (f x)
{-# inlineable runCC #-}

split :: Key (KeyState m) w -> DC m a b -> U.View (DC m w b) (SC m a w)
split p q = case unsnoc q of
  Empty -> U.Empty
  t :&: Del p' sk -> case testEquality p p' of
    Just Refl -> t U.:&: SC sk id
    Nothing -> case split p t of
      sk' U.:&: SC tl dl -> sk' U.:&: SC tl (dl `snoc` Del p' sk)
      U.Empty -> U.Empty

runCC_ :: MonadFail m => CC m a -> m a
runCC_ = runCC $ MonadFail.fail "missing prompt"
{-# inlineable runCC_ #-}

