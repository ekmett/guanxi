module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Cover.DLXSpec
import qualified Domain.IntervalSpec
import qualified FD.MonadSpec
import qualified Logic.ReflectionSpec
import qualified Prompt.IteratorSpec
import qualified Unaligned.BaseSpec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} $ do
  Cover.DLXSpec.spec
  Domain.IntervalSpec.spec
  FD.MonadSpec.spec
  Logic.ReflectionSpec.spec
  Prompt.IteratorSpec.spec
  Unaligned.BaseSpec.spec
