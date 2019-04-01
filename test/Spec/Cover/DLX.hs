module Spec.Cover.DLX where

import Cover.DLX
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "Cover.DLX" $
    it "solves" $ do
      x <- newCover 4 0
      r1 <- addOption x [0,1]      
      r2 <- addOption x [2,3]
      r3 <- addOption x [0,3]
      r4 <- addOption x [1,2]

      r1 `shouldBe` 4
      r2 `shouldBe` 6
      r3 `shouldBe` 8
      r4 `shouldBe` 10
      
      resultRef <- newIORef []
      solve x (\is -> modifyIORef resultRef (is:))
      result <- reverse <$> readIORef resultRef

      result `shouldBe` [[4,6], [8,10]]
