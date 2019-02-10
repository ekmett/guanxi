{-# language OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure)

import Logic.Reflection
import Unaligned.Internal

main :: IO ()
main = do
  result <- checkParallel tests
  unless result $
    exitFailure

tests :: Group
tests = Group "tests"
  [ ("Cat left id", catLeftId)
  , ("Cat right id", catRightId)
  , ("Cat assoc", catAssoc)
  ]

catLeftId = property $ do
  c <- forAll genCat'
  (mempty <> c) === c

catRightId = property $ do
  c <- forAll genCat'
  (c <> mempty) === c

catAssoc = property $ do
  a <- forAll genCat'
  b <- forAll genCat'
  c <- forAll genCat'
  let l = (a <> b) <> c
  let r = a <> (b <> c)
  when (l /= r) $ do
    liftIO (print l)
    liftIO (print r)
  l === r

scale' :: (Num a, Eq a) => a -> a
scale' x
  | x == 0    = 0
  | otherwise = x - 1

genCat :: Gen a -> Gen (Cat a)
genCat a = Gen.sized $ \s ->
  -- if s == 0 then pure E else
  Gen.frequency
    [ (1, pure E)
    , (4, C <$> a <*> Gen.scale scale' (genQ (genCat a)))
    ]

genCat' = Gen.prune $ genCat (Gen.int (Range.linear 0 10) :: Gen Int)

genQ :: Gen a -> Gen (Q a)
genQ a = Gen.sized $ \s ->
  -- if s == 0 then pure def else
  foldr cons def <$> Gen.list (Range.linear 0 5) (Gen.scale scale' a)
