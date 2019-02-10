module Logic.ReflectionSpec where

import Control.Applicative
import Logic.Reflection
import Test.Hspec


spec :: Spec
spec = do
  describe "Logic.Reflection" $ do
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
