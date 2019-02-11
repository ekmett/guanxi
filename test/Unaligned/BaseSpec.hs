{-# language OverloadedLists #-}
module Unaligned.BaseSpec where

import Data.Foldable
import Test.Hspec
import Unaligned.Internal

spec :: Spec
spec = do
  describe "Unaligned.Base" $ do
    describe "Cat" $ do
      describe "Monoid" $ do
        it "left id" $ do
          E <> [1,2,3] `shouldBe` ([1,2,3] :: Cat Integer)
        it "right id" $ do
          [4,5,6] <> E `shouldBe` ([4,5,6] :: Cat Integer)
        it "associativity" $ do
          let l = ([1,2,3] <>  [4,5,6]) <> [7,8,9]  :: Cat Integer
          let r =  [1,2,3] <> ([4,5,6]  <> [7,8,9]) :: Cat Integer
          toList l `shouldBe` toList r
      describe "linkAll" $ do
        it "preserves the elements" $ do
          let x = linkAll [[1,2,3],[4,5,6],[7,8,9] :: Cat Int]
          toList x `shouldBe` [1,2,3,4,5,6,7,8,9]
      describe "cons" $ do
        it "is consistent with list" $ do
          let x = cons 1 [2,3,4] :: Cat Int
          toList x `shouldBe` [1,2,3,4]
      describe "uncons" $ do
        it "splits apart cons" $ do
          let x = cons 1 [2,3,4] :: Cat Int
          uncons x `shouldBe` (1 :&: [2,3,4])
      describe "snoc" $ do
        it "is consistent with list" $ do
          let x = snoc [1,2,3] 4 :: Cat Int
          toList x `shouldBe` [1,2,3,4]

    describe "Q" $ do
      describe "cons" $ do
        it "is consistent with list" $ do
          let x = cons 1 [2,3,4] :: Q Int
          toList x `shouldBe` [1,2,3,4]
      describe "uncons" $ do
        it "splits apart cons" $ do
          let x = cons 1 [2,3,4] :: Q Int
          uncons x `shouldBe` (1 :&: [2,3,4])
      describe "snoc" $ do
        it "is consistent with list" $ do
          let x = snoc [1,2,3] 4 :: Q Int
          toList x `shouldBe` [1,2,3,4]
