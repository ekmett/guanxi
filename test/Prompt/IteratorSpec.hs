{-# language LambdaCase #-}
module Prompt.IteratorSpec where

import Data.Semigroup
import Data.Foldable
import Prompt.Class
import Prompt.Iterator
import Prompt.Reflection
import Test.Hspec hiding (example)
import Unaligned.Base

spec :: Spec
spec =
  describe "Prompt.Iterator" $
    it "iterates [1..5]" $ do
      let result = runPrompt example
      show result `shouldBe` "[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]"

example :: MonadPrompt m => m (Cat Int)
example = do
    i <- iterator $ forM_ [1..5]
    go Nil i
  where
    go l = \case
      Done -> return l
      Iterator a mi' -> mi' >>= go (l <> stimes a (singleton a))
