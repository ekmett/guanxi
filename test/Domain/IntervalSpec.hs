{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Domain.IntervalSpec where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.Reader
import Domain.Interval
import Signal
import Test.Hspec

spec :: Spec
spec = do
  describe "Domain.Interval" $ do
    describe "known" $ do
      it "known bottom = Nothing" $ do
        result <- signaller $
          bottom >>= known
        result `shouldBe` Nothing
      it "known [1..5] = Nothing" $ do
        result <- signaller $
          interval (Just 1) (Just 5) >>= known          
        result `shouldBe` Nothing
      it "known . abstract = Just" $ do
        result <- known (abstract 5)
        result `shouldBe` Just 5
    
    describe "negatei" $ do
      it "negates an interval" $ do
        result <- signaller $ do
          input <- interval (Just 1) (Just 5)
          r <- bottom
          negatei input r
          input `gtz` 4
          known r
        result `shouldBe` Just (-5)
      it "propagates information backwards" $ do
        result <- signaller $ do
          input <- interval (Just 1) (Just 5)
          r <- bottom
          negatei input r
          r `ltz` (-4)
          known input
        result `shouldBe` Just 5

newtype Signaller a = Signaller { getSignaller :: ReaderT (SignalEnv Signaller) IO a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadReader (SignalEnv Signaller), MonadIO)

runSignaller :: Signaller a -> SignalEnv Signaller -> IO a
runSignaller (Signaller r) = runReaderT r

signaller :: Signaller a -> IO a
signaller sa = do
  env <- newSignalEnv
  runSignaller sa env

instance PrimMonad Signaller where
  type PrimState Signaller = PrimState IO
  primitive = liftIO . primitive
