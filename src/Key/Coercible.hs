{-# language DefaultSignatures #-}
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
--
-- but it is left Coercible, this should be legal directly using things in base,
-- but we're currently missing a TestCoercion instance for STRefs

module Key.Coercible
  ( Key, newKey
  , Box(Lock), unlock
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Primitive.MutVar
import Data.Proxy
import Data.Type.Coercion
import Unsafe.Coerce

newtype Key m a = Key (MutVar (PrimState m) (Proxy a))
  deriving Eq

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
unlock k (Lock l x) = case testCoercion k l of
  Just Coercion -> Just $ coerce x
  Nothing -> Nothing
{-# inline unlock #-}
