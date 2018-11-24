{-# language LiberalTypeSynonyms #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language GADTs #-}

module Ref where

import Control.Monad.State.Class
import Control.Lens
import Data.IntMap hiding (Key)
import Data.Type.Equality
import Key

-- storing 'a' in here will leak the default value while the reference is alive,
-- but won't cause the environment to grow
data Ref u a = Ref a {-# unpack #-} !(Key u a) {-# unpack #-} !Int

data RefEnv u = RefEnv 
  { _refs     :: IntMap (Box u)
  , _freshRefId :: !Int
  }

makeClassy ''RefEnv

ref :: HasRefEnv s u => Ref u a -> Lens' s a
ref (Ref a k i) f = refs (at i f') where
  f' Nothing = Just . Lock k <$> f a
  f' (Just (Lock k' a')) = case testEquality k k' of 
     Just Refl -> Just . Lock k <$> f a'
     Nothing -> error "panic: bad ref env"

newRef :: (MonadState s m, MonadKey m, HasRefEnv s (KeyState m)) => a -> m (Ref (KeyState m) a)
newRef a = Ref a <$> newKey <*> (freshRefId <<+= 1)

readRef :: (MonadState s m, HasRefEnv s u) => Ref u a -> m a
readRef = use . ref

writeRef :: (MonadState s m, HasRefEnv s u) => Ref u a -> a -> m ()
writeRef r a = ref r .= a

modifyRef :: (MonadState s m, HasRefEnv s u) => Ref u a -> (a -> a) -> m ()
modifyRef r f = ref r %= f

-- delete a reference that we can prove somehow is not referenced anywhere
-- this will reset it to its 'default' value that was given when the ref was created
unsafeDeleteRef :: (MonadState s m, HasRefEnv s u) => Ref u a -> m ()
unsafeDeleteRef (Ref _ _ i) = refs.at i .= Nothing
