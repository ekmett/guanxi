{-# language OverloadedLists #-}
module Logic.ReflectionSpec where

import Control.Applicative
import Logic.Reflection
import Test.Hspec


spec :: Spec
spec = do
  describe "Logic.Reflection" $ do
    describe "view/unview" $ do
      it "empty" $ do
        let
          x :: Logic Int
          x = empty
          result = unview (view x)
        observeAll result `shouldBe` []
      it "empty on left" $ do
        let
          x :: Logic Int
          x = empty <|> pure 5
          result = unview (view x)
        observeAll result `shouldBe` [5]
      it "full" $ do
        let
          x :: Logic Int
          x = pure 1 <|> pure 2 <|> pure 3
          result = unview (view x)
        observeAll x `shouldBe` [1,2,3]
        observeAll result `shouldBe` [1,2,3]
    describe "Logic <|>" $ do
      it "left id" $ do
        let result = observeAll $ empty <|> pure 6
        result `shouldBe` [6 :: Integer]
      it "right id" $ do
        let result = observeAll $ pure 5 <|> empty
        result `shouldBe` [5 :: Integer]
      it "combining" $ do
        let result = observeAll $ pure 7 <|> pure 8
        result `shouldBe` [7,8]
      it "associativity" $ do
        let result1 = observeAll $ (pure 1 <|> pure 2) <|> pure 3
            result2 = observeAll $ pure 1 <|> (pure 2 <|> pure 3)
        (result1, result2) `shouldBe` ([1,2,3], [1,2,3])
