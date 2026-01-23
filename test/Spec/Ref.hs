{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Spec.Ref where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad

import Logic.Class
import qualified Logic.Cont as Cont
import qualified Logic.Naive as Naive
import qualified Logic.Reflection as Reflection

import Ref (MonadRef, newRef, readRef, writeRef)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  specFor "Cont"       Cont.observeAllT
  specFor "Naive"      Naive.observeAllT
  specFor "Reflection" Reflection.observeAllT

specFor :: (MonadLogic m, MonadRef m) => String -> (forall a. m a -> IO [a]) -> Spec
specFor s observeAllT = describe s $ do
  describe "writeRef" $ do
    it "backtracks a write followed by success" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (writeRef x 2 >> pure 2) <|> (readRef x)
        rs `shouldBe` [2, 1]

    it "backtracks a write followed by failure" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (writeRef x 2 >> empty) <|> (readRef x)
        rs `shouldBe` [1]

    it "backtracks a write followed by a choice" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            writeRef x 2
            (readRef x <|> readRef x)
        rs `shouldBe` [2,2]


  describe "once" $ do
    -- The following is debatable behaviour, but captures the current
    -- interaction between once and writeRef.  The issue is that once discards
    -- the failure continuation, but writeRef relies on the failure continuation
    -- being called in order to unwind the write.  Thus in this test case, the
    -- write is visible in both alternatives.
    --
    -- Prolog has cut (!) which is rather similar to once, in that it discards
    -- relevant choice points. However, Prolog implementations maintain separate
    -- representations of the choice point stack (like '<|>') and the trail (the
    -- addresses of references that have been written).  Thus even when cut is
    -- used to discard choice points, the trail can still be used to correctly
    -- revert writes when backtracking to an earlier choice point.
    --
    it "backtracks a write under once if it succeeds" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (once (writeRef x 2 >> pure 2)) <|> (readRef x)
        rs `shouldBe` [2,1] `butIs` [2,2]

    -- Within the local scope of a call to once, backtracking happens normally,
    -- so if the computation under once fails then the write ends up being
    -- backtracked as we would expect.
    it "backtracks a write-failure under once" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (once (writeRef x 2 >> empty)) <|> (readRef x)
        rs `shouldBe` [1]

    -- Here the computation under once succeeds so the unwind continuation is
    -- discarded before the failure. Thus the write is not backtracked.
    it "backtracks a write under once if failure happens later" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (once (writeRef x 2) >> empty) <|> (readRef x)
        rs `shouldBe` [1] `butIs` [2]

    -- Here the write under once cannot be backtracked, but the preceding write
    -- can be backtracked, so (somewhat counterintuitively) the read sees the
    -- original value of the reference.
    it "backtracks a write before once" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (writeRef x 3 >> once (writeRef x 2) >> empty) <|> (readRef x)
        rs `shouldBe` [1]

    it "does not backtrack two writes under once" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (once (writeRef x 2 <|> writeRef x 3) >> pure 0) <|> (readRef x)
        rs `shouldBe` [0,1] `butIs` [0,2]

    it "executes two writes" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            writeRef x 2 <|> writeRef x 3
            readRef x
        rs `shouldBe` [2,3]

    it "executes a write once" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            once (writeRef x 2 <|> writeRef x 3)
            readRef x
        rs `shouldBe` [2]


  describe "lnot" $ do
    it "backtracks a write under lnot" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (lnot (writeRef x 2) >> readRef x) <|> (readRef x)
        rs `shouldBe` [1] `butIs` [2]

    it "backtracks a write followed by failure under lnot" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (lnot (writeRef x 2 >> empty) >> readRef x) <|> (readRef x)
        rs `shouldBe` [1,1]

    it "backtracks a write under lnot . lnot" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (lnot (lnot (writeRef x 2)) >> readRef x) <|> (readRef x)
        rs `shouldBe` [1,1] `butIs` [2,2]

    it "backtracks both of two writes under lnot" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (lnot (writeRef x 2 <|> writeRef x 3) >> readRef x) <|> (readRef x)
        rs `shouldBe` [1] `butIs` [2]

    it "backtracks both of two writes under lnot . lnot" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (lnot (lnot (writeRef x 2 <|> writeRef x 3)) >> readRef x) <|> (readRef x)
        rs `shouldBe` [1,1] `butIs` [2,2]


  describe "ifte" $ do
    it "backtracks a write under ifte" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (ifte (writeRef x 2) (const (readRef x)) undefined) <|> (readRef x)
        rs `shouldBe` [2,1]

    it "backtracks a write when ifte fails" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (ifte (writeRef x 2 *> empty) undefined (readRef x)) <|> (readRef x)
        rs `shouldBe` [1,1]

    it "backtracks a write under once . ifte" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (once (ifte (writeRef x 2) (const (readRef x)) undefined)) <|> (readRef x)
        rs `shouldBe` [2,1] `butIs` [2,2]


  describe "msplit" $ do
    it "msplit >=> reflect is the identity" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (msplit >=> reflect) ((writeRef x 2 *> readRef x) <|> readRef x) <|> readRef x
        rs `shouldBe` [2,1,1]

    it "msplit >=> reflect cleans up" $ do
        rs <- observeAllT $ do
            x <- newRef (1 :: Int)
            (msplit >=> reflect) (readRef x <|> (writeRef x 2 >> readRef x)) <|> readRef x
        rs `shouldBe` [1,2,1]
  where
    -- The first argument is the "correct" result while
    -- the second is the result actually yielded by the implementation.
    butIs :: a -> a -> a
    butIs = const
    infix 5 `butIs`
