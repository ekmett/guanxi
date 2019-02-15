{-# language ForeignFunctionInterface #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language MultiWayIf #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language PatternSynonyms #-}

-- |
-- Copyright :  (c) Edward Kmett 2018-2019
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Dancing Links sped up with ZDDs

module Cover.DXZ
  ( Cover
  -- , ZDD
  , Node(..)
  , newCover, newCover_
  , addItems
  , addOptionalItems
  , addOption
  , count
  , solve, counts
  , Store(..), extract
  ) where

import Control.Lens (ifor_)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Foldable
import Data.Kind
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Primitive.Types
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable as Storable

data Store a = Store (PrimArray a) Int

data DXZ
type ZDD = Word32
type Item = Word32
type Option = Word32

-- free ZDD nodes
data Node = Node { option :: Option, lo :: Int, hi :: Int }

mask :: Word64
mask = unsafeShiftL 1 26 - 1

unpackNode :: Word64 -> Node
unpackNode n = Node
  (fromIntegral $ unsafeShiftR n 52)
  (fromIntegral $ unsafeShiftR n 26 .&. mask)
  (fromIntegral $ n .&. mask)

packNode :: Node -> Word64
packNode (Node k l h)
    = unsafeShiftL (fromIntegral k) 52
  .|. unsafeShiftL (fromIntegral l) 26
  .|. fromIntegral h

instance Storable Node where
  sizeOf _ = 8
  alignment _ = 8
  peek p = unpackNode <$> Storable.peek (castPtr p)
  poke p a = poke (castPtr p) (packNode a)

instance Prim Node where
  sizeOf# _ = 8#
  alignment# _ = 8#
  indexByteArray# b i = unpackNode (indexByteArray# b i)
  readByteArray# ba i s = case readByteArray# ba i s of
    (# s', a #) -> (# s', unpackNode a #)
  writeByteArray# ba i a s = writeByteArray# ba i (packNode a) s
  setByteArray# ba i j a s = setByteArray# ba i j (packNode a) s
  indexOffAddr# p i = unpackNode (indexOffAddr# p i)
  readOffAddr# p i s = case readOffAddr# p i s of
    (# s', a #) -> (# s', unpackNode a #)
  writeOffAddr# p i a s = writeOffAddr# p i (packNode a) s
  setOffAddr# p i j a s = setOffAddr# p i j (packNode a) s

newtype Cover (m :: Type -> Type) = Cover (ForeignPtr DXZ)

class PrimMonad m => HasCover m s | s -> m where
  cover :: s -> Cover m

instance PrimMonad m => HasCover m (Cover m) where
  cover = id

foreign import ccall unsafe "dxz_capi.h dxz_new" c_new :: Word32 -> Word32 -> IO (Ptr DXZ)
foreign import ccall unsafe "dxz_capi.h &dxz_delete" c_delete :: FunPtr (Ptr DXZ -> IO ())
foreign import ccall unsafe "dxz_capi.h dxz_add_items" c_add_items :: Ptr DXZ -> Word32 -> IO Item
foreign import ccall unsafe "dxz_capi.h dxz_add_optional_items" c_add_optional_items :: Ptr DXZ -> Word32 -> IO Item
foreign import ccall unsafe "dxz_capi.h dxz_add_option" c_add_option :: Ptr DXZ -> Ptr Item -> Word32 -> IO Option
foreign import ccall unsafe "dxz_capi.h dxz_solve" c_solve:: Ptr DXZ -> IO ZDD
foreign import ccall unsafe "dxz_capi.h dxz_heap" c_heap:: Ptr DXZ -> IO (Ptr Node)

newCover :: PrimMonad m => Int -> Int -> m (Cover m)
newCover n k = unsafeIOToPrim $ do
  dxz <- c_new (fromIntegral n) (fromIntegral k)
  Cover <$> newForeignPtr c_delete dxz

newCover_ :: PrimMonad m => m (Cover m)
newCover_ = newCover 0 0

withCover :: HasCover m s => s -> (Ptr DXZ -> IO r) -> m r
withCover (cover -> Cover d) f = unsafeIOToPrim $ withForeignPtr d f

addItems :: HasCover m s => s -> Int -> m Int -- Item
addItems d i = withCover d $ \p -> fromIntegral <$> c_add_items p (fromIntegral i)

addOptionalItems :: HasCover m s => s -> Int -> m Int
addOptionalItems d i = withCover d $ \p -> fromIntegral <$> c_add_optional_items p (fromIntegral i)

addOption :: HasCover m s => s -> [Int] -> m Option
addOption d is = withCover d $ \p -> do
  let n = length is
  allocaBytes (n * sizeOfItem) $ \ (q :: Ptr Item) -> do
    ifor_ is $ \i item -> pokeElemOff q i (fromIntegral item :: Item)
    c_add_option p q (fromIntegral n)

sizeOfItem :: Int
sizeOfItem = Storable.sizeOf (undefined :: Item)

solve :: HasCover m s => s -> m (Store Node)
solve d = withCover d $ \p -> do
  zdd <- fromIntegral <$> c_solve p
  h <- c_heap p
  ma <- newPrimArray $ zdd+1
  for_ [0..zdd] $ \i -> readOffPtr h i >>= writePrimArray ma i
  arr <- unsafeFreezePrimArray ma
  return $ Store arr zdd

extract :: Prim a => Store a -> a
extract (Store f s) = indexPrimArray f s

counts :: Store Node -> Store Int
counts (Store f s) = runST $ do
  let n = sizeofPrimArray f
  cs <- newPrimArray n
  for_ [0..n-1] $ \ i -> if
    | i <= 1 -> writePrimArray cs i i
    | Node _ l h <- indexPrimArray f i -> do 
      x <- readPrimArray cs l
      y <- readPrimArray cs h
      writePrimArray cs i (x + y)
  arr <- unsafeFreezePrimArray cs
  return $ Store arr s

count :: HasCover m s => s -> m Int
count = fmap (extract . counts) . solve 
