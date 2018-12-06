{-# language ViewPatterns #-}

module Back.Ref where

import Control.Applicative
import Control.Monad.Primitive
import Data.Primitive.MutVar

-- | morally, this brackets the success continuation with an undo operation to roll back with upon
-- taking the failure continuation
unwind
  :: (PrimMonad m, Alternative m)
  => (a -> (b, c))
  -> (c -> m d)
  -> m a
  -> m b
unwind f mu na = na >>= \a -> case f a of
  (b, c) -> pure b <|> (mu c *> empty)

-- | safely-backtracked mutvars
newtype BackRef s a = BackRef { getBackRef :: MutVar s a }

newBackRef :: PrimMonad m => a -> m (BackRef (PrimState m) a)
newBackRef = fmap BackRef . newMutVar

readBackRef :: PrimMonad m => BackRef (PrimState m) a -> m a
readBackRef = readMutVar . getBackRef

writeBackRef :: (PrimMonad m, Alternative m) => BackRef (PrimState m) a -> a -> m ()
writeBackRef (BackRef r) a' = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \ a -> (a', a)

updateBackRef :: (PrimMonad m, Alternative m) => BackRef (PrimState m) a -> (a -> (b, a)) -> m b
updateBackRef (BackRef r) f = unwind id (writeMutVar r) $ atomicModifyMutVar r $ \a@(f->(b,a'))->(a',(b,a))

updateBackRef' :: (PrimMonad m, Alternative m) => BackRef (PrimState m) a -> (a -> (b, a)) -> m b
updateBackRef' (BackRef r) f = unwind id (writeMutVar r) $ atomicModifyMutVar' r $ \a@(f->(b,a'))->(a',(b,a))

modifyBackRef :: (PrimMonad m, Alternative m) => BackRef (PrimState m) a -> (a -> a) -> m ()
modifyBackRef (BackRef r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \a -> (f a,a)

modifyBackRef' :: (PrimMonad m, Alternative m) => BackRef (PrimState m) a -> (a -> a) -> m ()
modifyBackRef' (BackRef r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar' r $ \a -> (f a,a)
