{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import GHC.Exts
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Exit (exitFailure)

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
  , ("Cat cons", catCons)
  , ("Cat snoc", catSnoc)
  , ("Cat singleton", catSingleton)
  , ("Cat uncons", catUncons)
  , ("Cat link", catLink)
  , ("Cat linkAll", catLinkAll)
  , ("Q cons", qCons)
  , ("Q snoc", qSnoc)
  , ("Q singleton", qSingleton)
  , ("Q uncons", qUncons)
  , ("Q toList/fromList", qToFromList)
  , ("Q fromList/toList", qFromToList)
  ]

catLeftId, catRightId, catAssoc, catCons, catSnoc, catSingleton, catUncons, catLink, catLinkAll :: Property
catLeftId = property $ do
  c <- forAll genCat
  (mempty <> c) === c

catRightId = property $ do
  c <- forAll genCat
  (c <> mempty) === c

catAssoc = property $ do
  a <- forAll genCat
  b <- forAll genCat
  c <- forAll genCat
  let l = (a <> b) <> c
  let r = a <> (b <> c)
  l ==== r

catCons = property $ do
  i <- forAll genInt
  c <- forAll genCat
  cons i c ===@ (i : toList c)

catSnoc = property $ do
  i <- forAll genInt
  c <- forAll genCat
  snoc c i ===@ (toList c ++ [i])

catSingleton = property $ do
  i <- forAll genInt
  (singleton i :: Cat Int) ===@ [i]

catUncons = property $ do
  c <- forAll genCat
  case uncons c of
    Empty   -> c === nil
    h :&: t -> do
      c ===@ h:toList t
      c ==== cons h t

catLink = property $ do
  i <- forAll genInt
  q <- forAll genQ
  c <- forAll genCat
  link i q c ===@ i : foldMap toList q ++ toList c

catLinkAll = property $ do
  q <- forAll genQ
  linkAll q ===@ foldMap toList q

qCons, qSnoc, qSingleton, qUncons, qToFromList, qFromToList:: Property
qCons = property $ do
  c <- forAll genCat
  q <- forAll genQ
  let expected = fromList (c : toList q)
  cons c q === expected

qSnoc = property $ do
  c <- forAll genCat
  q <- forAll genQ
  let expected = toList q ++ [c] :: [Cat Int]
  snoc q c ===@ expected

qSingleton = property $ do
  c <- forAll genCat
  let
    result :: Q (Cat Int)
    result = singleton c
  result === fromList [c]
  toList result === [c]

qUncons = property $ do
  q <- forAll genQ
  case uncons q of
    Empty   -> q === nil
    h :&: t -> do
      q === fromList (h : toList t)
      q === cons h t

qFromToList = property $ do
  q <- forAll genQ
  fromList (toList q) === q

qToFromList = property $ do
  l <- forAll (Gen.list (Range.linear 0 10) genCat)
  toList (fromList l :: Q (Cat Int)) === l
  

(===@) :: (Eq a, Show a, IsList (f a), Item (f a) ~ a) => f a -> [a] -> PropertyT IO ()
(===@) = (===) . toList
infix 4 ===@

(====) :: (Eq a, Show a, IsList (f a), Item (f a) ~ a) => f a -> f a -> PropertyT IO ()
(====) = (===) `on` toList 
infix 4 ====

genIsList :: (IsList (f a), Item (f a) ~ a, Monoid (f a)) => Range Int -> Gen a -> Gen (f a)
genIsList r a =
  let naive = fromList <$> Gen.list r a
  in  mconcat <$> Gen.list (Range.linear 0 10) naive

genQ :: Gen (Q (Cat Int))
genQ = fromList <$> Gen.list (Range.linear 0 10) genCat

genCat :: Gen (Cat Int)
genCat = genIsList (Range.linear 0 10) genInt

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 100)
