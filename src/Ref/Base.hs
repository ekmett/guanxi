{-# language LiberalTypeSynonyms #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language ViewPatterns #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- LogicT-compatible references
module Ref.Base
  ( Ref, RefEnv(..), HasRefEnv(..)
  , Reference(..)
  , ref, newRef, newSelfRef, readRef, writeRef, modifyRef, unsafeDeleteRef
  , refId
  ) where

import Control.Monad (guard)
import Control.Monad.State.Class
import Control.Lens
import Data.Coerce
import Data.Default
import Data.Hashable
import Data.Type.Coercion
import Ref.Env as Env
import Ref.Key

-- storing 'a' in here leaks the default value while the reference is alive,
-- but won't cause the explicit reference environment to grow at all
data Ref u a = Ref a {-# unpack #-} !(Cokey u a) {-# unpack #-} !Int

class Reference t u a | t -> u a where
  reference :: t -> Ref u a

instance Reference (Ref u a) u a where
  reference = id

-- use for hashing, etc.
refId :: Reference t u a => t -> Int
refId (reference -> Ref _ _ i) = i

instance Hashable (Ref u a) where
  hashWithSalt i (Ref _ _ j) = hashWithSalt i j
  hash (Ref _ _ j) = j

instance Eq (Ref u a) where
  Ref _ _ i == Ref _ _ j = i == j

instance Ord (Ref u a) where
  Ref _ _ i `compare` Ref _ _ j = compare i j

--instance TestEquality (Ref u) where
--  testEquality (Ref _ u i) (Ref _ v j) = guard (i == j) *> testEquality u v

instance TestCoercion (Ref u) where
  testCoercion (Ref _ u i) (Ref _ v j) = guard (i == j) *> testCoercion u v

data RefEnv u = RefEnv { _refs :: Env (Cobox u) }

instance Default (RefEnv u) where
  def = RefEnv def

makeClassy ''RefEnv

ref :: (HasRefEnv s u, Reference t u a) => t -> Lens' s a
ref (reference -> Ref a k i) f = refs (at i f') where
  f' Nothing = Just . Colock k <$> f a
  f' (Just (Colock k' a')) = case testCoercion k k' of
     Just Coercion -> Just . Colock k <$> f (coerce a')
     Nothing -> error "panic: bad ref"

newRef :: (MonadState s m, MonadKey m, HasRefEnv s (KeyState m)) => a -> m (Ref (KeyState m) a)
newRef a = Ref a <$> newCokey <*> (refs %%= allocate 1)

-- create a reference where the default value needs to know itself
-- this is cheaper than creating with a default and then updating
-- because it doesn't have to involve a write to the global environment
newSelfRef :: (MonadState s m, MonadKey m, HasRefEnv s (KeyState m)) =>
  (Ref (KeyState m) a -> a) -> m (Ref (KeyState m) a)
newSelfRef f = do
  i <- refs %%= allocate 1
  k <- newCokey
  let r = Ref (f r) k i in pure r

readRef :: (MonadState s m, HasRefEnv s u, Reference t u a) => t -> m a
readRef = use . ref

writeRef :: (MonadState s m, HasRefEnv s u, Reference t u a) => t -> a -> m ()
writeRef r a = ref r .= a

modifyRef :: (MonadState s m, HasRefEnv s u, Reference t u a) => t -> (a -> a) -> m ()
modifyRef r f = ref r %= f

-- delete a reference that we can prove somehow is not referenced anywhere
-- this will reset it to its 'default' value that was given when the ref was created
unsafeDeleteRef :: (MonadState s m, HasRefEnv s u, Reference t u a) => t -> m ()
unsafeDeleteRef (reference -> Ref _ _ i) = refs.at i .= Nothing
