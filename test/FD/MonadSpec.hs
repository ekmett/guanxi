{-# language OverloadedLists #-}

module FD.MonadSpec where

import FD.Monad
import FD.Var
import Test.Hspec hiding (example)

spec :: Spec
spec =
  describe "FD.Monad" $
    it "solves x <- [1..4], y <- [1..4], x < y" $ do
      let result = run example
      result `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

example :: FD s (Integer, Integer)
example = do
  x <- newFDVar [1..4]
  y <- newFDVar [1..4]
  lt x y
  (,) <$> val x <*> val y
