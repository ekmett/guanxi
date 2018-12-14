{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Unique where

import Control.Monad.Primitive
import Data.Hashable
import GHC.Prim
import GHC.Types

data Unique m = Unique !Int (MutableByteArray# (PrimState m))

instance Eq (Unique m) where
  Unique _ p == Unique _ q = isTrue# (sameMutableByteArray# p q)

instance Hashable (Unique m) where
  hash (Unique i _) = i
  hashWithSalt d (Unique i _) = hashWithSalt d i

newUnique :: PrimMonad m => m (Unique m)
newUnique = primitive $ \s -> case newByteArray# 0# s of
  (# s', ba #) -> (# s', Unique (I# (addr2Int# (unsafeCoerce# ba))) ba #)
