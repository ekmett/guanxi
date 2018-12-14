{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language RoleAnnotations #-}

-- |
-- Copyright :  (c) Edward Kmett 2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This construction is based on
-- <https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf The Key Monad: Type-Safe Unconstrained Dynamic Typing>
-- by Atze van der Ploeg, Koen Claessen, and Pablo Buiras

module Key
  ( Key, newKey
  , Box(Lock), unlock
  ) where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.Type.Coercion
import Data.Type.Equality
import Unsafe.Coerce

-- move to Equality.Key?
newtype Key m a = Key (MutVar (PrimState m) (Proxy a))
  deriving Eq

type role Key nominal nominal

instance TestEquality (Key m) where
  testEquality (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  {-# inline testEquality #-}

instance TestCoercion (Key m) where
  testCoercion (Key s :: Key m a) (Key t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

newKey :: PrimMonad m => m (Key m a)
newKey = Key <$> newMutVar Proxy
{-# inline newKey #-}

data Box m where
  Lock :: {-# unpack #-} !(Key m a) -> a -> Box m

unlock :: Key m a -> Box m -> Maybe a
unlock k (Lock l x) = case testEquality k l of
  Just Refl -> Just x
  Nothing -> Nothing
{-# inline unlock #-}
