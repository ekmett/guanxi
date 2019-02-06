module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Cover.DLXSpec
import qualified FD.MonadSpec
import qualified Prompt.IteratorSpec
import qualified Domain.IntervalSpec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} $ do
  Cover.DLXSpec.spec
  Domain.IntervalSpec.spec
  FD.MonadSpec.spec
  Prompt.IteratorSpec.spec
