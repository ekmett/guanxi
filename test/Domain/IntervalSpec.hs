{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Domain.IntervalSpec where

import Domain.Interval
import FD.Monad
import Test.Hspec

spec :: Spec
spec = do
  describe "Domain.Interval" $ do
    describe "known" $ do
      it "known bottom = Nothing" $ do
        let
          result = run $
            bottom >>= known
        result `shouldBe` [Nothing]
      it "known [1..5] = Nothing" $ do
        let
          result = run $
            interval (Just 1) (Just 5) >>= known
        result `shouldBe` [Nothing]
      it "known . abstract = Just" $ do
        let
          result = run $
            known (abstract 5)
        result `shouldBe` [Just 5]

    describe "negatei" $ do
      it "negates an interval" $ do
        let
          result = run $ do
            input <- interval (Just 1) (Just 5)
            r <- bottom
            negatei input r
            input `gtz` 4
            known r
        result `shouldBe` [Just (-5)]
      it "propagates information backwards" $ do
        let
          result = run $ do
            input <- interval (Just 1) (Just 5)
            r <- bottom
            negatei input r
            r `ltz` (-4)
            known input
        result `shouldBe` [Just 5]
