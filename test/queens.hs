{-# language ViewPatterns #-}
{-# language TupleSections #-}

module Main where

import Control.Monad.ST
import Cover
import Data.Bits
import Data.Foldable
import Data.STRef
import System.Exit

queens :: Int -> Int
queens n = runST $ do
  x <- newCover 0 0
  rows <- addItems x n
  cols <- addItems x n
  diag1 <- addOptionalItems x (n+n)
  diag2 <- addOptionalItems x (n+n)
  let organ i = fromIntegral $ unsafeShiftR (if odd i then n-1-i else n+i) 1
  for_ [0..n-1] $ \(organ -> r) ->
    for_ [0..n-1] $ \(organ -> c) ->
      addOption x [rows+r,cols+c,diag1+r+c,diag2+n-1-r+c]
  o <- newSTRef 0
  solve x $ \_ -> modifySTRef' o (1+)
  readSTRef o
 
main :: IO ()
main | queens 12 == 14200 = exitSuccess
     | otherwise = exitFailure
