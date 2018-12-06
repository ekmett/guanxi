{-# language ViewPatterns #-}

module Back.Ref where

import Back.Class
import Control.Monad.Primitive
import Data.Primitive.MutVar

-- | safely-backtracked ST references
newtype BackRef s a = BackRef { getBackRef :: MutVar s a }

newBackRef :: PrimMonad m => a -> m (BackRef (PrimState m) a)
newBackRef = fmap BackRef . newMutVar

readBackRef :: PrimMonad m => BackRef (PrimState m) a -> m a
readBackRef = readMutVar . getBackRef

writeBackRef :: MonadBack m => BackRef (PrimState m) a -> a -> m ()
writeBackRef (BackRef r) a' = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \ a -> (a', a)

updateBackRef :: MonadBack m => BackRef (PrimState m) a -> (a -> (b, a)) -> m b
updateBackRef (BackRef r) f = unwind id (writeMutVar r) $ atomicModifyMutVar r $ \a@(f->(b,a'))->(a',(b,a))

updateBackRef' :: MonadBack m => BackRef (PrimState m) a -> (a -> (b, a)) -> m b
updateBackRef' (BackRef r) f = unwind id (writeMutVar r) $ atomicModifyMutVar' r $ \a@(f->(b,a'))->(a',(b,a))

modifyBackRef :: MonadBack m => BackRef (PrimState m) a -> (a -> a) -> m ()
modifyBackRef (BackRef r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar r $ \a -> (f a,a)

modifyBackRef' :: MonadBack m => BackRef (PrimState m) a -> (a -> a) -> m ()
modifyBackRef' (BackRef r) f = unwind ((,)()) (writeMutVar r) $ atomicModifyMutVar' r $ \a -> (f a,a)
