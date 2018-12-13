{-# language ForeignFunctionInterface #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}

module Cover.DLX
  ( Cover
  , newCover, newCover_
  , addItems
  , addOptionalItems
  , addOption
  , each
  , solve
  , reset
  , count
  ) where

import Control.Applicative
import Control.Lens (ifor_)
import Control.Monad.Primitive
import Data.Kind
import Data.Traversable
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

data DLX
type Item = Word32
type Option = Word32

newtype Cover (m :: Type -> Type) = Cover (ForeignPtr DLX)

class PrimMonad m => HasCover m s | s -> m where
  cover :: s -> Cover m

instance PrimMonad m => HasCover m (Cover m) where
  cover = id

foreign import ccall unsafe "dlx_capi.h dlx_new" c_new :: Word32 -> Word32 -> IO (Ptr DLX)
foreign import ccall unsafe "dlx_capi.h &dlx_delete" c_delete :: FunPtr (Ptr DLX -> IO ())
foreign import ccall unsafe "dlx_capi.h dlx_add_items" c_add_items :: Ptr DLX -> Word32 -> IO Item
foreign import ccall unsafe "dlx_capi.h dlx_add_optional_items" c_add_optional_items :: Ptr DLX -> Word32 -> IO Item
foreign import ccall unsafe "dlx_capi.h dlx_add_option" c_add_option :: Ptr DLX -> Ptr Item -> Word32 -> IO Option
foreign import ccall unsafe "dlx_capi.h dlx_next" c_next :: Ptr DLX -> Ptr (Ptr Item) -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "dlx_capi.h dlx_reset" c_reset :: Ptr DLX -> IO ()
foreign import ccall unsafe "dlx_capi.h dlx_count" c_count :: Ptr DLX -> IO CInt

newCover :: PrimMonad m => Int -> Int -> m (Cover m)
newCover n k = unsafeIOToPrim $ do
  dlx <- c_new (fromIntegral n) (fromIntegral k)
  Cover <$> newForeignPtr c_delete dlx

newCover_ :: PrimMonad m => m (Cover m)
newCover_ = newCover 0 0

withCover :: HasCover m s => s -> (Ptr DLX -> IO r) -> m r
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

sizeOfPtr, sizeOfItem, sizeOfWord32 :: Int
sizeOfPtr = sizeOf (undefined :: Ptr Item)
sizeOfItem = sizeOf (undefined :: Item)
sizeOfWord32 = sizeOf (undefined :: Word32)

next :: HasCover m s => s -> m (Maybe [Int])
next d = withCover d $ \p -> allocaBytes (sizeOfPtr + sizeOfWord32) $ \q ->
  c_next p (castPtr q) (plusPtr q sizeOfPtr) >>= \ok -> if
    | ok /= 0 -> do
      (is :: Ptr Item) <- peekByteOff q 0
      (n :: Word32) <- peekByteOff q sizeOfPtr
      Just <$> for [0..n-1] (fmap fromIntegral . peekElemOff is . fromIntegral)
    | otherwise -> pure Nothing;

reset :: HasCover m s => s -> m ()
reset d = withCover d c_reset
 
-- NB: this "consumes" the cover. You can't use it until solve is finished.
each :: (HasCover m s, Alternative m) => s -> m [Int]
each d = next d >>= \case
  Nothing -> empty
  Just xs -> pure xs <|> each d

count :: HasCover m s => s -> m Int
count d = withCover d (fmap fromIntegral . c_count)

-- NB: this "consumes" the cover. You can't use it until solve is finished.
--
-- >>> x <- newCover 4 0
-- >>> addOption x [0,1]
-- 4
-- >>> addOption x [2,3]
-- 6
-- >>> addOption x [0,3]
-- 8
-- >>> addOption x [1,2]
-- 10
-- >>> solve x print
-- [4,6]
-- [8,10]
solve :: HasCover m s => s -> ([Int] -> m ()) -> m ()
solve d f = next d >>= \case
  Nothing -> pure ()
  Just xs -> f xs *> solve d f
