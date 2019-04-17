{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Spec.Domain.Interval where

import Control.Applicative
import Data.Foldable (traverse_)
import Domain.Internal
import FD.Monad
import Relative.Base (plus)
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
            1...5 >>= known
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
            input <- 1...5
            r <- bottom
            negatei input r
            input `gtz` 4
            known r
        result `shouldBe` [Just (-5)]
      it "propagates information backwards" $ do
        let
          result = run $ do
            input <- 1...5
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
        pair = 1...2
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
      -- it "[1..2] lt [1..2]" $ do
      --   let
      --     result = run $ do
      --       x <- pair; y <- pair
      --       x `lt` y
      --       knowns x y
      --   result `shouldBe` [(Just 1,Just 2)]
      -- it "[1..2] gt [1..2]" $ do
      --   let
      --     result = run $ do
      --       x <- pair
      --       y <- pair
      --       x `gt` y
      --       knowns x y
      --   result `shouldBe` [(Just 2,Just 1)]
      it "x = x+4" $ do
        let
          result = run $ do
            x <- 1...5
            x `eq` (x `plus` 4)
            concrete x
        result `shouldBe` []
      it "x+4 = x" $ do
        let
          result = run $ do
            x <- 1...5
            (x `plus` 4) `eq` x
            concrete x
        result `shouldBe` []
      it "x+3 = x+3" $ do
        let
          result = run $ do
            input <- 0...5
            let added = input `plus` 3
            eq added added
            concrete added
        result `shouldBe` [3,4,5,6,7,8]
      it "x = -x overlapping" $ do
        let
          result = run $ do
            input <- (-3)...4
            negd <- bottom
            negatei input negd
            eq input negd
            (,) <$> concrete input <*> concrete negd
        result `shouldBe` [(0,0)]
      it "x = -x non-overlaping" $ do
        let
          result = run $ do
            input <- 2...5
            negd <- bottom
            negatei input negd
            eq input negd
            (,) <$> concrete input <*> concrete negd
        result `shouldBe` []
      it "x = -x+5 (odd case) (no integer solution)" $ do
        let
          result = run $ do
            input <- 2...5 --solution is 2.5
            negd <- bottom
            negatei input negd
            let added = negd `plus` 5
            eq input added
            (,) <$> concrete input <*> concrete added
        result `shouldBe` []
      it "x = -x+4 (even case) with solution" $ do
        let
          result = run $ do
            input <- 1...5 --solution is 2
            negd <- bottom
            negatei input negd
            let added = negd `plus` 4
            eq input added
            (,) <$> concrete input <*> concrete added
        result `shouldBe` [(2,2)]
      it "-x+4 = x (even case) with solution" $ do
        let
          result = run $ do
            input <- 1...5 --solution is 2
            negd <- bottom
            negatei input negd
            let added = negd `plus` 4
            eq added input
            (,) <$> concrete input <*> concrete added
        result `shouldBe` [(2,2)]
      it "x-4 = -x (even case) with solution" $ do
        let
          result = run $ do
            input <- 1...5 --solution is 2
            negd <- bottom
            negatei input negd
            let added = input `plus` (-4)
            eq added negd
            (,) <$> concrete negd <*> concrete added
        result `shouldBe` [(-2,-2)]
      it "x = -x+4 (even case) without solution" $ do
        let
          result = run $ do
            input <- 3...5 --solution is 2
            negd <- bottom
            negatei input negd
            let added = negd `plus` 4
            eq input added
            (,) <$> concrete input <*> concrete added
        result `shouldBe` []
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
            y <- 1...2
            x `ne` y
            known y
        result `shouldBe` [Just 2]
      it "2 ne [1..2]" $ do
        let
          result = run $ do
            let x = abstract 2
            y <- 1...2
            x `ne` y
            known y
        result `shouldBe` [Just 1]
      -- it "[1..5] ne [1..5]" $ do
      --   let
      --     result = run $ do
      --       x <- 1...5
      --       y <- 1...5
      --       x `ne` y
      --       concrete x
      --   result `shouldBe` [1,2,3,4,5]
      it "[1..5] ne 5" $ do
        let
          result = run $ do
            x <- 1...5
            x `ne` abstract 5
            concrete x
        result `shouldBe` [1,2,3,4]
      it "3 zle [1..5]" $ do
        let
          result = run $ do
            x <- 1...5
            3 `zle` x
            concrete x
        result `shouldBe` [3,4,5]
      it "1 zle [1..5]" $ do
        let
          result = run $ do
            x <- 1...5
            1 `zle` x
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "5 zle [1..5]" $ do
        let
          result = run $ do
            x <- 1...5
            5 `zle` x
            known x
        result `shouldBe` [Just 5]
      it "6 zle [1..5]" $ do
        let
          result = run $ do
            x <- 1...5
            6 `zle` x
            concrete x
        result `shouldBe` []
      it "0 zle [1..5]" $ do
        let
          result = run $ do
            x <- 1...5
            0 `zle` x
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 3" $ do
        let
          result = run $ do
            x <- 1...5
            x `lez` 3
            concrete x
        result `shouldBe` [1,2,3]
      it "[1..5] lez 1" $ do
        let
          result = run $ do
            x <- 1...5
            x `lez` 1
            concrete x
        result `shouldBe` [1]
      it "[1..5] lez 5" $ do
        let
          result = run $ do
            x <- 1...5
            x `lez` 5
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 6" $ do
        let
          result = run $ do
            x <- 1...5
            x `lez` 6
            concrete x
        result `shouldBe` [1,2,3,4,5]
      it "[1..5] lez 0" $ do
        let
          result = run $ do
            x <- 1...5
            x `lez` 0
            concrete x
        result `shouldBe` []

    describe "<|>" $ do
      it "right id" $ do
        let
          result = run $
            pure 3 <|> empty
        result `shouldBe` [3 :: Integer]
      it "left id" $ do
        let
          result = run $
            empty <|> pure 4
        result `shouldBe` [4 :: Integer]

    -- Art of the Propagator section 6.3
    describe "baker cooper fletcher miller and smith" $ do
      it "finds a solution" $ do
        let
          result = run $ do
            let
              floors = 1...5
              allDistinct [] = pure ()
              allDistinct (x:xs) = traverse_ (ne x) xs *> allDistinct xs
              notAdjacent a b = a `lt` plus b (-1) <|> a `gt` plus b 1

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
            notAdjacent c f
            notAdjacent s f

            traverse concrete [b,c,f,m,s]

        result `shouldBe` [[3,2,4,5,1]]
