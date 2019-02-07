{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Domain.IntervalSpec where

import Data.Foldable (traverse_)
import Domain.Internal
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

    describe "absi" $ do
      it "passes through a positive number unchanged" $ do
        let
          result = run $ do
            let input = abstract 20
            output <- bottom
            absi input output
            known output
        result `shouldBe` [Just 20]
      it "negates a negative number" $ do
        let
          result = run $ do
            let input = abstract (-30)
            output <- bottom
            absi input output
            known output
        result `shouldBe` [Just 30]

    describe "comparisons" $ do
      let
        pair :: FD s (Interval (FD s))
        pair = interval (Just 1) (Just 2)
        knowns a b = (,) <$> known a <*> known b
        concretes a b = (,) <$> concrete a <*> concrete b
      it "[1..2] eq [1..2]" $ do
        let
          result = run $ do
            x <- pair; y <- pair
            x `eq` y
            concretes x y
        result `shouldBe` [(1,1), (2,2)]
      it "[1] eq [1..2]" $ do
        let
          result = run $ do
            let x = abstract 1
            y <- pair
            x `eq` y
            knowns x y
        result `shouldBe` [(Just 1, Just 1)]
      it "[1..2] eq [2]" $ do
        let
          result = run $ do
            x <- pair
            let y = abstract 2
            x `eq` y
            knowns x y
        result `shouldBe` [(Just 2, Just 2)]
      it "[1..2] lt [1..2]" $ do
        let
          result = run $ do
            x <- pair; y <- pair
            x `lt` y
            knowns x y
        result `shouldBe` [(Just 1,Just 2)]
      it "[1..2] gt [1..2]" $ do
        let
          result = run $ do
            x <- pair
            y <- pair
            x `gt` y
            knowns x y
        result `shouldBe` [(Just 2,Just 1)]
      it "[1] le [1..2]" $ do
        let
          result = run $ do
            let x = abstract 1
            y <- pair
            x `le` y
            knowns x y
        result `shouldBe` [(Just 1, Nothing)]
      it "[1..2] le [1]" $ do
        let
          result = run $ do
            x <- pair
            let y = abstract 1
            x `le` y
            knowns x y
        result `shouldBe` [(Just 1, Just 1)]
      it "1 ne [1..2]" $ do
        let
          result = run $ do
            let x = abstract 1
            y <- interval (Just 1) (Just 2)
            x `ne` y
            known y
        result `shouldBe` [Just 2]
      it "2 ne [1..2]" $ do
        let
          result = run $ do
            let x = abstract 2
            y <- interval (Just 1) (Just 2)
            x `ne` y
            known y
        result `shouldBe` [Just 1]
      it "[1..5] ne 1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            y <- interval (Just 1) (Just 5)
            x `ne` y
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] ne 5" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `ne` abstract 5
            concrete x
        result `shouldBe` [1,2,3,4]
      it "3 zle [1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            3 `zle` x
            concrete x
        result `shouldBe` [3,4,5]
      it "1 zle [1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            1 `zle` x
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "5 zle [1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            5 `zle` x
            concrete x
        result `shouldBe` [5]
      it "6 zle [1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            6 `zle` x
            concrete x
        result `shouldBe` []
      it "0 zle [1..5]" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            0 `zle` x
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 3" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `lez` 3
            concrete x
        result `shouldBe` [1,2,3]
      it "[1..5] lez 1" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `lez` 1
            concrete x
        result `shouldBe` [1]
      it "[1..5] lez 5" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `lez` 5
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 6" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `lez` 6
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 0" $ do
        let
          result = run $ do
            x <- interval (Just 1) (Just 5)
            x `lez` 0
            concrete x
        result `shouldBe` []


    -- Art of the Propagator section 6.3
    describe "baker cooper fletcher miller and smith" $ do
      it "finds a solution" $ do
        let
          result = run $ do
            let
              floors = interval (Just 1) (Just 5)
              allDistinct [] = pure ()
              allDistinct (x:xs) = traverse_ (ne x) xs *> allDistinct xs

            -- make all tenants
            b <- floors
            c <- floors
            f <- floors
            m <- floors
            s <- floors

            -- Nobody lives on the same floor
            allDistinct [b,c,f,m,s]

            -- constraints
            b `nez` 5
            c `nez` 1
            f `nez` 5
            f `nez` 1
            m `gt` c

            -- cooper does not live directly above or below fletcher
            -- smith does not live directly above or below fletcher
            onceKnown f $ \i -> do
              c `nez` (i+1)
              c `nez` (i-1)
              s `nez` (i+1)
              s `nez` (i-1)
            onceKnown c $ \i -> do
              f `nez` (i+1)
              f `nez` (i-1)
            onceKnown s $ \i -> do
              f `nez` (i+1)
              f `nez` (i-1)

            traverse concrete [b,c,f,m,s]

        result `shouldBe` [[3,2,4,5,1]]
