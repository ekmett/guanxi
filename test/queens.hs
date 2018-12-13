{-# language ViewPatterns #-}
{-# language TupleSections #-}

module Main where

import Control.Monad (unless)
import Control.Monad.ST
import Cover.DXZ
import Data.Foldable
import System.Exit

queens :: Int -> Int
queens n = runST $ do
  x <- newCover_
  rows <- addItems x n
  cols <- addItems x n
  diag1 <- addOptionalItems x (n+n)
  diag2 <- addOptionalItems x (n+n)
  let organ i = fromIntegral $ div (if odd i then n-1-i else n+i) 2
  for_ [0..n-1] $ \(organ -> r) ->
    for_ [0..n-1] $ \(organ -> c) ->
      addOption x [rows+r,cols+c,diag1+r+c,diag2+n-1-r+c]
  count x
 
main :: IO ()
main = do
  let n = queens 12
  print n
  unless (n == 14200) exitFailure
