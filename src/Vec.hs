{-# language BlockArguments #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}
module Vec where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.Types
import Data.Primitive.PrimArray
import Data.Primitive.MutVar
import Ref

-- transient
data Vec s a = Vec {-# unpack #-} !Int {-# unpack #-} !(MutablePrimArray s a)

-- non-backtracking writes by default

-- newtype Vec s a = Vec (MVar s (Slab s a))

pattern DEFAULT_SIZE :: Int
pattern DEFAULT_SIZE = 4

newVec_ :: (PrimMonad m, Prim a) => m (Vec (PrimState m) a)
newVec_ = newVec DEFAULT_SIZE

newVec :: (PrimMonad m, Prim a) => Int -> m (Vec (PrimState m) a)
newVec n = stToPrim do Vec 0 <$> newPrimArray n
{-# inline newVec #-}

addVec :: (PrimMonad m, Prim a) => a -> Vec (PrimState m) a -> m (Int, Vec (PrimState m) a)
addVec a (Vec i pa) = stToPrim do
  n <- getSizeofMutablePrimArray pa
  if i < n then do
    writePrimArray pa i a
    return (i, Vec (i+1) pa)
  else do
    pa' <- resizeMutablePrimArray pa (n*2) 
    writePrimArray pa' i a
    return (i, Vec (i+1) pa')
{-# inline addVec #-}

subVec :: (PrimMonad m, Prim a) => Vec (PrimState m) a -> m (Vec (PrimState m) a)
subVec (Vec i pa) = stToPrim do
  n <- getSizeofMutablePrimArray pa
  let n' = unsafeShiftR n 2
  if i >= n' then return $ Vec (i-1) pa
  else Vec (i-1) <$> resizeMutablePrimArray pa (n*2)

readVec :: (PrimMonad m, Prim a) => Vec (PrimState m) a -> Int -> m a
readVec (Vec _ pa) i = readPrimArray pa i
{-# inline readVec #-}

-- doesn't change shape
writeVec :: (PrimMonad m, Prim a) => Vec (PrimState m) a -> Int -> a -> m ()
writeVec (Vec _ pa) i a = writePrimArray pa i a
{-# inline writeVec #-}

sizeVec :: Vec s a -> Int
sizeVec (Vec i _ ) = i
{-# inline sizeVec #-}

-- this would play the role of std::vector, non-transient non-thread-safe version
newtype Vector s a = Vector (MutVar s (Vec s a))

newVector :: (PrimMonad m, Prim a) => Int -> m (Vector (PrimState m) a)
newVector n = stToPrim do
  v <- newVec n
  Vector <$> newMutVar v
{-# inline newVector #-}

-- not thread safe
nonAtomicModifyVector :: PrimMonad m => Vector (PrimState m) a -> (Vec (PrimState m) a -> ST (PrimState m) (r, Vec (PrimState m) a)) -> m r
nonAtomicModifyVector (Vector ref) k = stToPrim do
  v <- readMutVar ref
  (r, v') <- k v
  r <$ writeMutVar ref v'
{-# inline nonAtomicModifyVector #-}

modifyVector :: PrimMonad m => Vector (PrimState m) a -> (Vec (PrimState m) a -> ST (PrimState m) (Vec (PrimState m) a)) -> m ()
modifyVector (Vector ref) k = stToPrim $ (readMutVar ref >>= k) >>= writeMutVar ref
{-# inline modifyVector #-}
  
addVector :: (PrimMonad m, Prim a) => a -> Vector (PrimState m) a -> m Int
addVector a v = nonAtomicModifyVector v \vec -> addVec a vec
{-# inline addVector #-}

subVector :: (PrimMonad m, Prim a) => Vector (PrimState m) a -> m ()
subVector v = modifyVector v subVec
{-# inline subVector #-}

readVector :: (PrimMonad m, Prim a) => Vector (PrimState m) a -> Int -> m a
readVector (Vector ref) i = readMutVar ref >>= \(Vec _ pa) -> readPrimArray pa i
{-# inline readVector #-}

writeVector :: (PrimMonad m, Prim a) => Vector (PrimState m) a -> Int -> a -> m ()
writeVector (Vector ref) i a = readMutVar ref >>= \vec -> writeVec vec i a
{-# inline writeVector #-}

sizeVector :: PrimMonad m => Vector (PrimState m) a -> m Int
sizeVector (Vector ref) = stToPrim $ sizeVec <$> readMutVar ref
{-# inline sizeVector #-}

-- safe backtracking operations:
--
-- newVector
-- readVector
-- addBackVector
-- writeBackVector
-- sizeVector

addBackVector :: (MonadRef m, Prim a) => a -> Vector (PrimState m) a -> m Int
addBackVector a v = unwind (,()) (\_->subVector v) $ stToPrim $ addVector a v
{-# inline addBackVector #-}

writeBackVector :: (MonadRef m, Prim a) => Vector (PrimState m) a -> Int -> a -> m a -- returns old value
writeBackVector v@(Vector ref) i a = unwind (\x -> (x,x)) (writeVector v i) $ do
  vec <- readMutVar ref
  old <- readVec vec i
  old <$ writeVec vec i a
{-# inline writeBackVector #-}
