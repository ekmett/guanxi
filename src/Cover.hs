{-# language ForeignFunctionInterface #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}

module Cover
  ( Cover
  , newCover
  , addItems
  , addOptionalItems
  , addOption
  , each
  , solve
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

newCover :: PrimMonad m => Int -> Int -> m (Cover m)
newCover n k = unsafeIOToPrim $ do
  dlx <- c_new (fromIntegral n) (fromIntegral k)
  Cover <$> newForeignPtr c_delete dlx

withCover :: HasCover m s => s -> (Ptr DLX -> IO r) -> m r
withCover (cover -> Cover d) f = unsafeIOToPrim $ withForeignPtr d f

addItems :: HasCover m s => s -> Int -> m Item
addItems d i = withCover d $ \p -> fromIntegral <$> c_add_items p (fromIntegral i)

addOptionalItems :: HasCover m s => s -> Int -> m Item
addOptionalItems d i = withCover d $ \p -> fromIntegral <$> c_add_optional_items p (fromIntegral i)

addOption :: HasCover m s => s -> [Item] -> m Option
addOption d is = withCover d $ \p -> do
  let n = length is
  allocaBytes (n * sizeOfItem) $ \ (q :: Ptr Item) -> do
    ifor_ is $ \i item -> pokeElemOff q i item 
    c_add_option p q (fromIntegral n)

sizeOfPtr, sizeOfItem, sizeOfWord32 :: Int
sizeOfPtr = sizeOf (undefined :: Ptr Item)
sizeOfItem = sizeOf (undefined :: Item)
sizeOfWord32 = sizeOf (undefined :: Word32)

next :: HasCover m s => s -> m (Maybe [Item])
next d = withCover d $ \p -> allocaBytes (sizeOfPtr + sizeOfWord32) $ \q ->
  c_next p (castPtr q) (plusPtr q sizeOfPtr) >>= \ok -> if
    | ok /= 0 -> do
      (is :: Ptr Item) <- peekByteOff q 0
      (n :: Word32) <- peekByteOff q sizeOfPtr
      Just <$> for [0..n-1] (peekElemOff is . fromIntegral)
    | otherwise -> pure Nothing;

reset :: HasCover m s => s -> m ()
reset d = withCover d c_reset
 
-- NB: this "consumes" the cover. You can't use it until solve is finished.
each :: (HasCover m s, Alternative m) => s -> m [Item]
each d = next d >>= \case
  Nothing -> empty
  Just xs -> pure xs <|> each d

-- NB: this "consumes" the cover. You can't use it until solve is finished.
solve :: HasCover m s => s -> ([Item] -> m ()) -> m ()
solve d f = next d >>= \case
  Nothing -> pure ()
  Just xs -> f xs *> solve d f
